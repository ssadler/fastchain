{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Node
  ( runNode
  ) where

import Control.Concurrent

import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import Data.Time

import Database.Fastchain.Chain
import Database.Fastchain.Crypto
import Database.Fastchain.Http
import Database.Fastchain.Logging
import Database.Fastchain.Prelude
import Database.Fastchain.Types

import qualified Data.Text as T


runNode :: Node -> IO ()
runNode node = do
  forkIO $ runHttp node
  forkIO $ runChainify node
  forever $ do
    req <- takeMVar $ _server node
    handle node req


handle :: Node -> NodeQuery -> IO ()
handle node (PushTx tx) = pushTx node tx

handle node (AddPeer peerId server) = do
  let peers = _peers node
   in modifyMVar_ peers $ pure . (& at peerId .~ Just server)

handle node (SignTimestamp tx t out) =
  onTimestamp node tx t >>= push out

handle node (RelayTx stx sigs) = onTimestampedTx node stx sigs


-- | Get a timestamed transaction, check and write to backlog
onTimestampedTx :: Node -> STX -> SigMap -> IO ()
onTimestampedTx node (t,tx@(Tx txid _)) sigs = do
  peers <- readMVar $ _peers node
  let payload = txidPayload txid t
      (sigsValid,_,_) = checkSigs peers payload sigs
  when sigsValid $ do
    modifyMVar_ (_backlog node) $ pure . (& at t .~ Just tx)
    infoN node $ "Writing tx to backlog"


-- | Calculate whether or not a map of node keys -> signatures meets
-- requirements
checkSigs :: Peers -> ByteString -> SigMap -> (Bool, Int, Int)
checkSigs peers payload sigs = do
  let neededSigs = 1 + floor (fromIntegral (length peers) / 2 :: Double)
      goodSig (pk,sig) = Map.member pk peers && verify pk payload sig
      numValid = length $ filter goodSig $ toList sigs
   in (neededSigs <= numValid, neededSigs, numValid)


txidPayload :: Txid -> UTCTime -> ByteString
txidPayload txid time = encodeUtf8 txid <> " " <> C8.pack (show time)


-- | Client posts new transaction
pushTx :: Node -> Transaction -> IO ()
pushTx node tx@(Tx txid _) = do
  let info msg = infoN node $ "onPostTransaction:" <> txid <> ": " <> msg
  -- Sign and timestamp the tx
  time <- getCurrentTime
  let payload = txidPayload txid time
  info $ "Signing tx payload: " <> decodeUtf8 payload
  let sig = nodeSign node $ txidPayload txid time

  -- Ask other nodes to sign timestamp
  peers <- readMVar $ _peers node
  info $ T.pack $ "Asking " ++ show (Map.size peers) ++ " to sign"
  msigs <- forM (toList peers) $ \(peerId, server) -> do
      msig <- request server $ SignTimestamp txid time
      pure $ (,) peerId <$> msig
  let sigs = fromList $ (_pubkey node, sig) : catMaybes msigs
  
  -- If enough sigs, broadcast once again.
  let (sigsPass, needed, valid) = checkSigs peers payload sigs
  info $ "Got sigs: " <> T.pack (show (needed, valid))
  when sigsPass $
   forM_ (toList peers) $ \(_, server) ->
     push server $ RelayTx (time,tx) sigs


-- | Got a relayed transaction. Validate timestamp and respond with sig.
onTimestamp :: Node -> Txid -> UTCTime -> IO (Maybe Signature)
onTimestamp node txid t = do
  time <- getCurrentTime
  let ndiff = diffUTCTime time t
  let diff = realToFrac ndiff :: Double
  infoN node $ T.pack $ "Got timestamp request: " ++ show ndiff
  if diff > 0 && diff < 0.01
     then do
       infoN node $ "signing timestamp"
       pure $ Just $ nodeSign node $ txidPayload txid t
     else do
      infoN node $ "timestamp out of bounds"
      pure Nothing


nodeSign :: Node -> ByteString -> Signature
nodeSign n = sign (_secret n) (_pubkey n)
