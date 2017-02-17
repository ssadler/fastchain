{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Advisory
  ( runAdvisory
  ) where

import Control.Concurrent

import qualified Data.Binary as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time

import Database.Fastchain.Chain
import Database.Fastchain.Crypto
import Database.Fastchain.Logging
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import qualified System.ZMQ4 as Z


data Actions m = Actions
  { takeServer' :: m NodeQuery
  , getTime' :: m UTCTime
  , broadcast' :: BroadcastMessage -> m ()
  , sign' :: ByteString -> Signature
  , writeTx' :: STX -> m Int64
  }


ioActions :: Node -> Actions IO
ioActions node =
  Actions (takeMVar $ _server node)
          getCurrentTime
          (Z.send' (_pubSock node) [] . B.encode)
          (nodeSign node)
          (db node insertTxs . (:[]))


runAdvisory :: Node -> IO ()
runAdvisory node = do
  --forkIO $ runHttp node
  forkIO $ runChainify node
  let actions = ioActions node
  forever $ do
    req <- takeServer' actions
    handle actions req


handle :: Monad m => Actions m -> NodeQuery -> m ()
handle actions (PushTx tx) = pushTx actions tx

-- | Client posts new transaction
pushTx :: Monad m => Actions m -> Transaction -> m ()
pushTx acts tx@(Tx xid _) = do
  -- Sign and timestamp the tx
  time <- getTime' acts
  let sig = sign' acts $ txidPayload xid time
      msg = TxAdvisory (time, tx) sig
  writeTx' acts (time, tx)
  broadcast' acts msg


-- | Calculate whether or not a map of node keys -> signatures meets
-- requirements
checkSigs :: Peers -> ByteString -> SigMap -> (Bool, Int, Int)
checkSigs peers payload sigs = do
  let neededSigs = 1 + floor (fromIntegral (length peers) / 2 :: Double)
      goodSig (pk,sig) = Map.member pk peers && verify pk payload sig
      numValid = length $ filter goodSig $ toList sigs
   in (neededSigs <= numValid, neededSigs, numValid)


txidPayload :: Txid -> UTCTime -> ByteString
txidPayload xid time = toStrict $ encode (xid,time)


-- | Got a relayed transaction. Validate timestamp and respond with sig.
onTimestamp :: Node -> Txid -> UTCTime -> IO (Maybe Signature)
onTimestamp node xid t = do
  time <- getCurrentTime
  let ndiff = diffUTCTime time t
  let diff = realToFrac ndiff :: Double
  infoN node $ T.pack $ "Got timestamp request: " ++ show ndiff
  if diff > 0 && diff < 0.01
     then do
       infoN node $ "signing timestamp"
       pure $ Just $ nodeSign node $ txidPayload xid t
     else do
      infoN node $ "timestamp out of bounds"
      pure Nothing


nodeSign :: Node -> ByteString -> Signature
nodeSign node = let (p,s) = keyPair' $ _config node
                 in sign s p
