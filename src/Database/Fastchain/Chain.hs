{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Chain where


import Control.Concurrent

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Database.Fastchain.Logging
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types


data Chainify m = Chainify
  { getSpentOf' :: [Txid] -> m (Set Txid)
  , insertTxs' :: [STX] -> m Int64
  , delay' :: m ()
  , getMature' :: UTCTime -> m [STX]
  }


ioChainify :: Node -> Chainify IO
ioChainify node =
  Chainify (db node getSpentOf)
           (db node insertTxs)
           (threadDelay 1000000)
           gmt
  where
    gmt old = modifyMVar (_backlog node)
                         (pure . getMatureTxs old)


runChainify :: Node -> IO ()
runChainify node = forever $ do
  old <- addUTCTime (-1) <$> getCurrentTime
  processBacklog (ioChainify node) old


processBacklog :: Monad m => Chainify m -> UTCTime -> m ()
processBacklog ch old = do
  txs <- getMature' ch old
  if null txs
     then delay' ch
     else chainify ch txs


-- add transactions to chain. transactions should be in order.
chainify :: Monad m => Chainify m -> [(UTCTime, Transaction)] -> m ()
chainify ch txs = do
  nonds <- noDoubleSpends ch txs
  insertTxs' ch nonds
  pure ()


-- spanAntitone in containers 0.5.10
getMatureTxs :: UTCTime -> Backlog -> (Backlog, [(UTCTime, Transaction)])
getMatureTxs old backlog =
  case Map.toAscList backlog of
    [] -> (backlog, [])
    ((t, tx):_) -> do
      if t < old
         then
           let nbl = backlog & at t .~ Nothing
               (nnbl, ntxs) = getMatureTxs old nbl
            in (nnbl, (t,tx):ntxs)
         else
           (backlog, [])


noDoubleSpends :: Monad m => Chainify m -> [STX] -> m [STX]
noDoubleSpends validate txs = do
  let allSpends = nub $ concat [s | (_,Tx _ s) <- txs]
  spent <- getSpentOf' validate allSpends
  let nondouble = nonDoubleSpent spent txs
  pure nondouble


nonDoubleSpent :: (Set Txid) -> [STX] -> [STX]
nonDoubleSpent _ [] = []
nonDoubleSpent spent (stx@(_, Tx _ spends):rest) =
  let spending = Set.fromList spends
      allSpends = Set.union spending spent
   in if null $ Set.intersection spent spending
         then stx : nonDoubleSpent allSpends rest
         else nonDoubleSpent spent rest
  
