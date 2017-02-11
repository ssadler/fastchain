{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Node
  ( runNode
  ) where

import Control.Concurrent

import Control.Monad.Trans.Reader

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
    runReaderT (handle req) node


handle :: NodeQuery -> RunNode ()
handle (PostTx tx) = onPostTransaction tx

handle (AddPeer peerId server) = do
  peers <- _peers <$> ask
  lift $ modifyMVar_ peers $ pure . (& at peerId .~ Just server)

handle (PlzTimestamp tx t out) = onRelayTransaction tx t >>= push out

handle (HasTimestamp tx t sigs) = onTimestampedTx tx t sigs


-- | Get a timestamp, check and write to backlog
onTimestampedTx :: Transaction -> UTCTime -> SigMap -> RunNode ()
onTimestampedTx tx t sigs = do
  peers <- (_peers <$> ask) >>= lift . readMVar
  let sigsValid = sigsAreGood peers (C8.pack $ show t) sigs
  when sigsValid $ do
    mbacklog <- _backlog <$> ask
    lift $ modifyMVar_ mbacklog $ pure . (& at t .~ Just tx)


-- | Calculate whether or not a map of node keys -> signatures meets
-- requirements
sigsAreGood :: Peers -> ByteString -> SigMap -> Bool
sigsAreGood peers payload sigs = do
  let neededSigs = 1 + floor (fromIntegral (length peers) / 2 :: Double)
      goodSig (pk,sig) = Map.member pk peers && verify pk payload sig
      numValid = length $ filter goodSig $ toList sigs
   in neededSigs <= numValid


onPostTransaction :: Transaction -> RunNode ()
onPostTransaction tx@(Tx txid _) = do
  -- Sign and timestamp the tx
  time <- lift getCurrentTime
  sig <- signTxTime txid time
  pk <- _pubkey <$> ask

  -- Ask other nodes to sign timestamp
  peers <- (_peers <$> ask) >>= lift . readMVar
  msigs <- forM (toList peers) $ \(peerId, server) -> do
      msig <- request server $ PlzTimestamp txid time
      pure $ (,) peerId <$> msig
  let sigs = fromList $ (pk, sig) : catMaybes msigs
  
  -- If enough sigs, broadcast once again.
  -- TODO: verify
  let neededSigs = ceiling (fromIntegral (length peers) / 2 :: Double)
  if length sigs >= neededSigs
     then forM_ (toList peers) $ \(_, server) ->
            push server $ HasTimestamp tx time sigs
     else return ()


-- | Got a relayed transaction. Validate timestamp and respond with sig.
onRelayTransaction :: Txid -> UTCTime -> RunNode (Maybe Signature)
onRelayTransaction txid t = do
  time <- lift getCurrentTime
  if t < time && addUTCTime 1 t >= time
     then Just <$> nodeSign (encodeUtf8 txid)
     else pure Nothing


signTxTime :: Txid -> UTCTime -> RunNode Signature
signTxTime txid time = nodeSign $ encodeUtf8 txid <> " " <> C8.pack (show time)


nodeSign :: ByteString -> RunNode Signature
nodeSign payload = do
  n <- ask
  pure $ sign (_secret n) (_pubkey n) payload
