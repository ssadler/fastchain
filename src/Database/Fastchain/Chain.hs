{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Chain where


import Control.Concurrent

import Data.Map.Strict as Map

import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types


runChainify :: Node -> IO ()
runChainify node = forever $ do
  modifyMVar_ (_backlog node) $ \bl ->
    runReaderT (chainify bl) node
  threadDelay 1000


chainify :: Backlog -> RunNode Backlog
chainify backlog = do
  ct <- lift getCurrentTime
  case Map.toAscList backlog of
    ((t, tx):_) ->
      if diffUTCTime ct t >= 1
         then do chainifyTx t tx
                 chainify $ backlog & at t .~ Nothing
         else pure backlog
    _ -> pure backlog


chainifyTx :: UTCTime -> Transaction -> RunNode ()
chainifyTx t tx = do
  pure ()
