{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Backlog where

import qualified Data.Map.Strict as Map

import Database.Fastchain.Node
import Database.Fastchain.Prelude
import Database.Fastchain.Types


type BacklogE m = Effectful BacklogEffects m

data BacklogEffects m = BE
  { broadcastTxs' :: [STX] -> BacklogE m ()
  , delay' :: BacklogE m ()
  , getMature' :: UTCTime -> BacklogE m [STX]
  }


ioBacklog :: Node -> BacklogEffects IO
ioBacklog node =
  BE (lift . mapM_ (onMatureTx node))
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
     else eff1 broadcastTxs' txs


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

