{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Node
  ( runNode
  ) where

import Control.Concurrent

import Control.Monad.Trans.Reader

import qualified Data.ByteString.Char8 as C8
import Data.Map.Strict as Map (fromList, toList, size, keysSet)
import qualified Data.Set as Set
import Data.Time

import Lens.Micro
import Lens.Micro.Platform ()

import Database.Fastchain.Crypto
import Database.Fastchain.Http
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types
import Database.Fastchain.Utils


runNode :: Node -> IO ()
runNode = runReaderT $ do
  ask >>= lift . forkIO . runHttp
  forever $ do
    server <- _server <$> ask
    req <- lift $ takeMVar server
    handle req


handle :: NodeQuery -> RunNode ()
handle (PostTx tx) = onPostTransaction tx

handle (AddPeer peerId server) = do
  peers <- _peers <$> ask
  lift $ modifyMVar_ peers $ pure . (& at peerId .~ Just server)

handle (PlzTimestamp tx t out) = onRelayTransaction tx t >>= push out

handle (HasTimestamp tx t sigs) = onTimestampedTx tx t sigs


onTimestampedTx :: Transaction -> UTCTime -> SigMap -> RunNode ()
onTimestampedTx tx t sigs = do
  sigsValid <- sigsAreGood (C8.pack $ show t) sigs
  lift (print sigs)


-- | Calculate whether or not a map of node keys -> signatures meets
-- requirements
sigsAreGood :: ByteString -> SigMap -> RunNode Bool
sigsAreGood payload theirSigs = do
  peerKeys <- keysSet <$> ((_peers <$> ask) >>= lift . readMVar)
  let sigs = restrictKeys theirSigs peerKeys
  let neededSigs = 1 + floor (fromIntegral (Set.size peerKeys) / 2 :: Double)
  if size sigs <= neededSigs
     then pure False
     else pure $ all (\(pk,sig) -> verify pk payload sig) $ toList sigs


onPostTransaction :: Transaction -> RunNode ()
onPostTransaction tx = do
  -- Sign and timestamp the tx
  time <- lift getCurrentTime
  sig <- signTxTime tx time
  pk <- _pubkey <$> ask

  -- Ask other nodes to sign timestamp
  peers <- (_peers <$> ask) >>= lift . readMVar
  msigs <- forM (toList peers) $ \(peerId, server) -> do
      msig <- request server $ PlzTimestamp tx time
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
onRelayTransaction :: Transaction -> UTCTime -> RunNode (Maybe Signature)
onRelayTransaction tx t = do
  time <- lift getCurrentTime
  if t < time && addUTCTime 1 t >= time
     then Just <$> signTx tx
     else pure Nothing


signTxTime :: Transaction -> UTCTime -> RunNode Signature
signTxTime (Tx txid _) time = nodeSign $ encodeUtf8 txid <> " " <> C8.pack (show time)


nodeSign :: ByteString -> RunNode Signature
nodeSign payload = do
  n <- ask
  pure $ sign (_secret n) (_pubkey n) payload


signTx :: Transaction -> RunNode Signature
signTx (Tx txid _) = nodeSign $ encodeUtf8 txid
