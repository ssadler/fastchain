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
import Database.Fastchain.Prelude
import Database.Fastchain.Types


runNode :: Node -> IO ()
runNode node = do
  forkIO $ runHttp node
  forkIO $ runChainify node
  forever $ do
    req <- takeMVar $ _server node
    handle node req


handle :: Node -> NodeQuery -> IO ()
handle node (PostTx tx) = onPostTransaction node tx

handle node (AddPeer peerId server) = do
  let peers = _peers node
   in modifyMVar_ peers $ pure . (& at peerId .~ Just server)

handle node (PlzTimestamp tx t out) =
  onRelayTransaction node tx t >>= push out

handle node (HasTimestamp tx t sigs) = onTimestampedTx node tx t sigs


-- | Get a timestamp, check and write to backlog
onTimestampedTx :: Node -> Transaction -> UTCTime -> SigMap -> IO ()
onTimestampedTx node tx t sigs = do
  peers <- readMVar $ _peers node
  let sigsValid = sigsAreGood peers (C8.pack $ show t) sigs
  when sigsValid $
    modifyMVar_ (_backlog node) $ pure . (& at t .~ Just tx)


-- | Calculate whether or not a map of node keys -> signatures meets
-- requirements
sigsAreGood :: Peers -> ByteString -> SigMap -> Bool
sigsAreGood peers payload sigs = do
  let neededSigs = 1 + floor (fromIntegral (length peers) / 2 :: Double)
      goodSig (pk,sig) = Map.member pk peers && verify pk payload sig
      numValid = length $ filter goodSig $ toList sigs
   in neededSigs <= numValid


onPostTransaction :: Node -> Transaction -> IO ()
onPostTransaction node tx@(Tx txid _) = do
  -- Sign and timestamp the tx
  time <- getCurrentTime
  let sig = signTxTime node txid time

  -- Ask other nodes to sign timestamp
  peers <- readMVar $ _peers node
  msigs <- forM (toList peers) $ \(peerId, server) -> do
      msig <- request server $ PlzTimestamp txid time
      pure $ (,) peerId <$> msig
  let sigs = fromList $ (_pubkey node, sig) : catMaybes msigs
  
  -- If enough sigs, broadcast once again.
  -- TODO: verify
  let neededSigs = ceiling (fromIntegral (length peers) / 2 :: Double)
  if length sigs >= neededSigs
     then forM_ (toList peers) $ \(_, server) ->
            push server $ HasTimestamp tx time sigs
     else return ()


-- | Got a relayed transaction. Validate timestamp and respond with sig.
onRelayTransaction :: Node -> Txid -> UTCTime -> IO (Maybe Signature)
onRelayTransaction node txid t = do
  time <- getCurrentTime
  if t < time && addUTCTime 1 t >= time
     then pure $ Just $ nodeSign node (encodeUtf8 txid)
     else pure Nothing


signTxTime :: Node -> Txid -> UTCTime -> Signature
signTxTime node txid time = nodeSign node $
    encodeUtf8 txid <> " " <> C8.pack (show time)


nodeSign :: Node -> ByteString -> Signature
nodeSign n payload = sign (_secret n) (_pubkey n) payload
