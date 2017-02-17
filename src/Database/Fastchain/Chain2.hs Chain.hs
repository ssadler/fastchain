module Database.Fastchain.Chain where

import Data.Binary
import Data.List (sortOn)

import Database.Fastchain.Crypto
import Database.Fastchain.Hub
import Database.Fastchain.Prelude
import Database.Fastchain.Types

import System.ZMQ4


type Chainify m = Effectful ChainEffects m


data ChainEffects m = ChainEffects
  { yieldTx' :: [ITX] -> Chainify m ()
  , delay' :: Int -> Chainify m ()
  , updateHead' :: (PublicKey, MVar ITX) -> Chainify m QueueHead
  }


ioChainEffects :: HubInterface -> ChainEffects IO
ioChainEffects node =
  ChainEffects
    (lift . putMVar (_router node) . ConsiderTx)
    (lift . threadDelay)
    (\(pk,mvar) -> (,) <$> lift (tryTakeMVar mvar) <*> pure (pk,mvar))


runChainifyIO :: Node -> IO ()
runChainifyIO node = do
  let peers = toList $ _peers node
  inputs <- traverseOf (each . _2) mkForwarder' peers
  let heads = [(Nothing, i) | i <- inputs]
  runEffects (chainify heads) (ChainEffects undefined undefined undefined)
  where
    mkForwarder' :: Socket Sub -> IO (MVar ITX)
    mkForwarder' sock = do
          mvar <- newEmptyMVar
          forkIO $ forever $ do
            bs <- receive sock
            -- this is not going to work because the socket is receiving broadcast messages
            putMVar mvar (decode $ fromStrict bs)
          pure mvar


type QueueHead = (Maybe ITX, (PublicKey, MVar ITX))


chainify :: Monad m => [QueueHead] -> Chainify m ()
chainify heads = do
  let (nothings,mrest) = span (isNothing . fst) heads
      rest@((stx,_):_) = fromJust . fst <$> mrest
      agree = takeWhile ((==stx) . fst) rest
      nAgree = if null mrest then 0 else length agree

  when (nAgree > 0) $ eff1 yieldTx' agree

  let toUpdate = map snd $ nothings ++ take nAgree mrest
  updated <- mapM (eff1 updateHead') toUpdate
  let newHeads = updated ++ drop nAgree mrest :: [QueueHead]
  chainify $ sortOn actionable newHeads


actionable :: QueueHead -> Maybe STX
actionable = fmap fst . fst
