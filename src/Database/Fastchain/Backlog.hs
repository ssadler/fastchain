{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Backlog where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types


type BacklogE m = Effectful BacklogEffects m

data BacklogEffects m = BE
  { getSpentOf' :: [Txid] -> BacklogE m (Set Txid)
  , broadcastTxs' :: [STX] -> BacklogE m ()
  , delay' :: BacklogE m ()
  , getMature' :: UTCTime -> BacklogE m [STX]
  }


ioBacklog :: Node -> BacklogEffects IO
ioBacklog node =
  BE (lift . db node getSpentOf)
     (lift . mapM_ (putMVar (_hub node) . MatureTx))
     (lift $ threadDelay 10000)
     (lift . gmt)
  where
    gmt old = modifyMVar (_backlog node)
                         (pure . getMatureTxs old)


runBacklog :: Node -> IO ()
runBacklog node = forever $ do
  old <- addUTCTime (-1) <$> getCurrentTime
  runEffects (processBacklog old) (ioBacklog node)


processBacklog :: Monad m => UTCTime -> BacklogE m ()
processBacklog old = do
  txs <- eff1 getMature' old
  if null txs
     then eff delay'
     else yieldMature txs


-- yield mature transactions
yieldMature :: Monad m => [STX] -> BacklogE m ()
yieldMature txs = do
  nonds <- noDoubleSpends txs
  eff1 broadcastTxs' nonds


-- spanAntitone in containers 0.5.10
getMatureTxs :: UTCTime -> Backlog -> (Backlog, [STX])
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


noDoubleSpends :: Monad m => [STX] -> BacklogE m [STX]
noDoubleSpends txs = do
  let allSpends = nub $ concat [s | (_,Tx _ s) <- txs]
  spent <- eff1 getSpentOf' allSpends
  pure $ nonDoubleSpent spent txs


nonDoubleSpent :: Set Txid -> [STX] -> [STX]
nonDoubleSpent _ [] = []
nonDoubleSpent spent (stx@(_, Tx _ spends):rest) =
  let spending = Set.fromList spends
      allSpends = Set.union spending spent
   in if null $ Set.intersection spent spending
         then stx : nonDoubleSpent allSpends rest
         else nonDoubleSpent spent rest
