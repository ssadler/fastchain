{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Node where

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types


makeNode :: Config -> MVar NodeQuery -> IO Node
makeNode c h = Node c h <$> newMVar mempty


runNode :: Node -> (BroadcastMessage -> IO ()) -> IO ()
runNode (Node conf hub backlog) bcast = forever $ do
  msg <- takeMVar hub
  case msg of
       ClientTx tx -> do
         t <- getCurrentTime
         bcast $ TxAdvisory (t,tx)
       AdviseTx (t,tx) -> do
         -- TODO: validate within time bounds
         modifyMVar_ backlog $ pure . (& at t .~ Just tx)
       MatureTx stx@(_,Tx (Txid txid) _) -> do
         let sig = sign (_keyPair conf) txid
         bcast $ TxInclusion (stx,sig)
       CheckAgreeTx (stx,sm) -> do
         if elect conf (stx,sm)
            then print (stx, length sm)
            else print "noelect"


elect :: Config -> (STX, SigMap) -> Bool
elect _ = (>0) . length . snd
