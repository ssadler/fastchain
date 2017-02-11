{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Chain where


import Control.Concurrent

import qualified Data.Map.Strict as Map

import Database.Fastchain.Logging
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import qualified Data.Text as T


runChainify :: Node -> IO ()
runChainify node = forever $ do
  ct <- getCurrentTime
  txs <- modifyMVar (_backlog node) $ pure . getMatureTxs ct
  infoN node $ T.pack $ show $ length txs
  if null txs
     then threadDelay 1000000
     else chainify node txs


-- spanAntitone in containers 0.5.10
getMatureTxs :: UTCTime -> Backlog -> (Backlog, [(UTCTime, Transaction)])
getMatureTxs ct backlog =
  case Map.toAscList backlog of
    [] -> (backlog, [])
    ((t, tx):_) -> do
      if diffUTCTime ct t > 1
         then
           let nbl = backlog & at t .~ Nothing
               (nnbl, ntxs) = getMatureTxs ct nbl
            in (nnbl, (t,tx):ntxs)
         else
           (backlog, [])


-- add transactions to chain. transactions should be in order.
chainify :: Node -> [(UTCTime, Transaction)] -> IO ()
chainify node txs = do
  -- Filter double spends according to db
  let allSpends = concat [s | (_,Tx _ s) <- txs]
  spent <- db node getSpentOf allSpends
  infoN node $ T.pack $ show spent

-- transactions > 1 second old get written to DB

chainifyTx :: Node -> UTCTime -> Transaction -> IO ()
chainifyTx node t tx = do
  pure ()
