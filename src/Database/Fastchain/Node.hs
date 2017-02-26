{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Node where

import Control.Monad.Trans.Except

import Database.Fastchain.App
import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types



makeNode :: Config -> IO Node
makeNode conf = do
  mBroadcast <- newEmptyMVar
  backlog <- newMVar mempty
  let node = Node { _config = conf
                  , _backlog = backlog
                  , _broadcast = putMVar mBroadcast
                  , _broadcastOut = mBroadcast
                  }
  pure node


pushTx :: Node -> Transaction -> IO ()
pushTx node tx = do
   t <- getCurrentTime
   _broadcast node $ TxAdvisory (t,tx)


onAdviseTx :: Node -> STX -> IO ()
onAdviseTx node (t,tx) = do
  -- TODO: validate within time bounds
  modifyMVar_ (_backlog node) $ pure . (& at t .~ Just tx)


onMatureTx :: Node -> STX -> IO ()
onMatureTx node stx@(_,tx) = do
  let keyPair = _keyPair $ _config node
      sig = sign keyPair $ encodeUtf8 $ _txid tx
  _broadcast node $ TxInclusion (stx,sig)


onVotedTx :: Node -> (STX, SigMap) -> IO ()
onVotedTx node (stx,sm) = do
  let needed = length $ _peers $ _config node
  print $ length sm
  print $ needed
  if needed == length sm
     then do
       r <- runExceptT (runTx node stx)
       case r of
           Left err -> print err
           Right _ -> pure ()
     else do
       print "dropped tx"


elect :: Config -> (STX, SigMap) -> Bool
elect _ = (>0) . length . snd
