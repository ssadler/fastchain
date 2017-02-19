{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Database.Fastchain.Zip where

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types


type Zip m = Effectful ZipEffects m


data QueueHead = QH
  { _mitx :: Maybe ITX
  , _pk :: PublicKey
  , _chan :: Chan ITX
  } deriving (Eq, Show)


data ZipEffects m = ZipEffects
  { yieldTx' :: (STX, SigMap) -> Zip m ()
  , delay' :: Zip m ()
  , updateHead' :: QueueHead -> Zip m QueueHead
  , zipify' :: [QueueHead] -> Zip m ()
  }


runZipIO :: MVar NodeQuery -> [(PublicKey, Chan ITX)] -> IO ()
runZipIO hub feeds =
  let heads = uncurry (QH Nothing) <$> feeds
  in runEffects (zipify heads) effects
  where
  effects = ZipEffects
    (lift . putMVar hub . CheckAgreeTx)
    (lift $ threadDelay 10000)
    (\(QH _ pk chan) -> QH <$> tryReadChan chan <*> pure pk <*> pure chan)
    zipify
  tryReadChan chan = lift $ do
    empty <- isEmptyChan chan
    if empty then pure Nothing
             else Just <$> readChan chan


zipify :: Monad m => [QueueHead] -> Zip m ()
zipify heads = do
  let (nothings,mrest) = span (isNothing . _mitx) heads
      stxs@((stx,_):_) = [(s,(pk,sig)) | (QH (Just (s,sig)) pk _) <- mrest]
      agree = takeWhile ((==stx) . fst) stxs
      nAgree = if null mrest then 0 else length agree

  if nAgree == 0
     then eff delay'
     else eff1 yieldTx' (stx,snd <$> agree)

  let toUpdate = nothings ++ take nAgree mrest
  updated <- forM toUpdate $ eff1 updateHead'
  let newHeads = updated ++ drop nAgree mrest
  eff1 zipify' $ sortActionable newHeads


sortActionable :: [QueueHead] -> [QueueHead]
sortActionable = sortOn (fmap fst . _mitx)
