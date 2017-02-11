{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Chain where


import Control.Concurrent

import Data.Map.Strict as Map

import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types


runChainify :: Node -> IO ()
runChainify node = forever $ do
  modifyMVar_ (_backlog node) $ \bl -> chainify node bl
  threadDelay 1000


chainify :: Node -> Backlog -> IO Backlog
chainify node backlog = do
  ct <- getCurrentTime
  case Map.toAscList backlog of
    ((t, tx):_) ->
      if diffUTCTime ct t >= 1
         then do chainifyTx node t tx
                 chainify node $ backlog & at t .~ Nothing
         else pure backlog
    _ -> pure backlog


chainifyTx :: Node -> UTCTime -> Transaction -> IO ()
chainifyTx node t tx = do
  pure ()
