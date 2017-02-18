module Database.Fastchain.Zip where

import Data.List (sortOn)

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types

type Zip m = Effectful ZipEffects m
type Feed = (PublicKey, Chan ITX)
type QueueHead = (Maybe ITX, Feed)


data ZipEffects m = ZipEffects
  { yieldTx' :: [ITX] -> Zip m ()
  , delay' :: Int -> Zip m ()
  , updateHead' :: Feed -> Zip m QueueHead
  }


runZipIO :: MVar NodeQuery -> [Feed] -> IO ()
runZipIO hub feeds =
  let heads = (,) Nothing <$> feeds
  in runEffects (zipify heads) effects
  where
  effects = ZipEffects
    (lift . putMVar hub . CheckAgreeTx)
    (lift . threadDelay)
    (\(pk,chan) -> (,) <$> tryReadChan chan <*> pure (pk,chan))
  tryReadChan chan = lift $ do
    empty <- isEmptyChan chan
    if empty then pure Nothing
             else Just <$> readChan chan


zipify :: Monad m => [QueueHead] -> Zip m ()
zipify heads = do
  let (nothings,mrest) = span (isNothing . fst) heads
      rest@((stx,_):_) = fromJust . fst <$> mrest
      agree = takeWhile ((==stx) . fst) rest
      nAgree = if null mrest then 0 else length agree

  if nAgree == 0
     then eff1 delay' 10000
     else eff1 yieldTx' agree

  let toUpdate = map snd $ nothings ++ take nAgree mrest
  updated <- mapM (eff1 updateHead') toUpdate
  let newHeads = updated ++ drop nAgree mrest :: [QueueHead]
  zipify $ sortOn actionable newHeads


actionable :: QueueHead -> Maybe STX
actionable = fmap fst . fst
