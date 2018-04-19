{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Node where

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Monitors
import Database.Fastchain.Schema
import Database.Fastchain.Types



makeNode :: Config -> Monitors -> IO Node
makeNode conf ekg = do
  mBroadcast <- newEmptyMVar
  backlog <- newMVar mempty
  pool <- dbPool conf
  exec <- newChan
  pure Node { _config = conf
            , _backlog = backlog
            , _broadcast = putMVar mBroadcast
            , _broadcastOut = mBroadcast
            , _dbPool = pool
            , _exec = exec
            , _monitors = ekg
            }

-- | Transaction enters the system via web api etc
--   Is timestamped and passed along to other nodes
pushTx :: Node -> Transaction -> IO ()
pushTx node tx = do
   t <- getCurrentTime
   _broadcast node $ TxAdvisory (t,tx)
   incPushTx $ _monitors node


-- | Received timestamped transaction which now awaits
--   maturity
onAdviseTx :: Node -> STX -> IO ()
onAdviseTx node (t,tx) = do
  -- TODO: validate within time bounds
  modifyMVar_ (_backlog node) $ pure . (& at t .~ Just tx)
  incAdviseTx $ _monitors node


-- | Transaction is mature, is now broadcast for inclusion
onMatureTx :: Node -> STX -> IO ()
onMatureTx node stx@(_,tx) = do
  let keyPair = _keyPair $ _config node
      sig = sign keyPair $ _txid tx
  _broadcast node $ TxInclusion (stx,sig)
  incMatureTx $ _monitors node


onVotedTx :: Node -> (STX, SigMap) -> IO ()
onVotedTx node (stx,sm) = do
  let needed = length $ _peers $ _config node
  if needed <= length sm
     then do
       writeChan (_exec node) stx
     else do
       print "dropped tx"


elect :: Config -> (STX, SigMap) -> Bool
elect _ = (>0) . length . snd
