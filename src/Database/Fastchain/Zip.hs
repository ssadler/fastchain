{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Fastchain.Zip where

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types

import Data.Binary
import qualified Data.ByteString as BS
import Data.List (partition)

import System.IO.Unsafe
import System.ZMQ4 hiding (Subscriber)

import Debug.Trace

type Zip m = Effectful ZipEffects m

instance Show (Socket Sub) where
  show = ('#':) . show . unsafePerformIO . fileDescriptor

instance Eq (Socket Sub) where
  s == s' = let fd = unsafePerformIO . fileDescriptor
             in fd s == fd s'


data QueueHead = QH
  { _mitx :: Maybe ITX
  , _pk :: PublicKey
  , _sock :: Socket Sub
  } deriving (Eq, Show)


data ZipEffects m = ZipEffects
  { yieldTx' :: (STX, SigMap) -> Zip m ()
  , delay' :: Zip m ()
  , poll' :: Int64 -> [Socket Sub] -> Zip m [[Event]]
  , receive' :: Socket Sub -> Zip m ByteString
  , zipify' :: [QueueHead] -> Zip m ()
  , getCurrentTime' :: Zip m UTCTime
  , updateHeads' :: [QueueHead] -> Zip m [QueueHead]
  , log' :: Priority -> String -> Zip m ()
  }

instance Monad m => Logs (Zip m) where
  getTime' = eff getCurrentTime'
  logE = eff2 log'


runZipIO :: [(PublicKey, Socket Sub)] -> ((STX,SigMap) -> IO ()) -> IO ()
runZipIO feeds onSignedTx =
  let heads = [QH Nothing pk s | (pk,s) <- feeds]
  in runEffects (zipify heads) effects
  where
  effects = ZipEffects
    (lift . onSignedTx)
    (lift $ threadDelay 10001)
    (\a b -> lift $ pollIO a b)
    (lift . receive)
    zipify
    (lift getCurrentTime)
    updateHeads
    (\p m -> lift $ logM "zip" p m)
  pollIO ms = poll ms . fmap (\s -> Sock s [In,Err] Nothing)


updateHeads :: Monad m => [QueueHead] -> Zip m [QueueHead]
updateHeads oldHeads = do
  maxtime <- addUTCTime 1 <$> eff getCurrentTime'
  let pollMore heads = do
        d <- diffUTCTime maxtime <$> eff getCurrentTime'
        if d <= 0
           then pure heads
           else do
             let socks = _sock <$> heads
             results <- zip heads <$> eff2 poll' 0 socks
             newHeads <- forM results $ uncurry updateHead
             when (newHeads == heads) $ eff delay'
             let (done,need) = partition (isJust . _mitx) newHeads
             if null need then pure done
                          else (done++) <$> pollMore need
  pollMore oldHeads


updateHead :: Monad m => QueueHead -> [Event] -> Zip m QueueHead
updateHead qh [] = pure qh {_mitx = Nothing}
updateHead qh [In] = do
  bs <- eff1 receive' $ _sock qh
  let itx = decode $ fromStrict $ BS.drop 1 bs
  pure qh {_mitx = Just itx}
updateHead qh xs = do
  traceShow xs $ pure qh


zipify :: Monad m => [QueueHead] -> Zip m ()
zipify heads = do
  let (nothings,mrest) = span (isNothing . _mitx) heads
      stxs@((stx,_):_) = [(s,(pk,sig)) | (QH (Just (s,sig)) pk _) <- mrest]
      agree = takeWhile ((==stx) . fst) stxs
      nAgree = if null mrest then 0 else length agree

  -- nAgree will be 0 if no peers are offering transactions
  if nAgree == 0
     then eff delay'
     else eff1 yieldTx' (stx,snd <$> agree)

  let toUpdate = nothings ++ take nAgree mrest
  updated <- eff1 updateHeads' toUpdate
  let newHeads = updated ++ drop nAgree mrest
  eff1 zipify' $ sortActionable newHeads


sortActionable :: [QueueHead] -> [QueueHead]
sortActionable = sortOn (fmap fst . _mitx)
