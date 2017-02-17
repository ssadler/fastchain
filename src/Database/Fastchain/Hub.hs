{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Hub where

import Data.Binary

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types

import System.ZMQ4


data HubInterface = HubInterface 
  { _hub :: MVar NodeQuery
  , _feeds :: [(PublicKey, MVar ITX)]
  }


withHub :: Config -> (HubInterface -> IO a) -> IO a
withHub (Config _ peers _) act = do
  hub <- newEmptyMVar
  withContext $ \ctx -> do
    withSocket ctx Pub $ \_ -> do
      feeds <- traverseOf (each . _2) (toPeerFeed ctx hub) $ toList peers
      act $ HubInterface hub feeds


toPeerFeed :: Context -> MVar NodeQuery -> String -> IO (MVar ITX)
toPeerFeed ctx hub addr = do
  feed <- newEmptyMVar
  forkIO $
    withSocket ctx Sub $ \sock -> do
      connect sock addr
      forever $ do
        bs <- fromStrict <$> receive sock
        case decode bs of
             TxAdvisory stx -> putMVar hub (AdviseTx stx)
             TxInclusion itx -> putMVar feed itx
  pure feed
