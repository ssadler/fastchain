{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Hub where

import Data.Binary

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types

import System.ZMQ4


data HubInterface = HubInterface 
  { _hhub :: MVar NodeQuery
  , _feeds :: [(PublicKey, Chan ITX)]
  , _broadcast :: BroadcastMessage -> IO ()
  }


withHub :: Config -> (HubInterface -> IO a) -> IO a
withHub (Config (me,_) peers port _) act = do
  hub <- newEmptyMVar
  withContext $ \ctx -> do
    withSocket ctx Pub $ \pub -> do
      let peersAndMe = nub $ (me,port) : peers
          addr = "tcp://127.0.0.1:" ++ show port
      bind pub addr
      feeds <- traverseOf (each . _2) (toPeerFeed ctx hub) peersAndMe
      let broadcast = send' pub [] . encode
      act $ HubInterface hub feeds broadcast


toPeerFeed :: Context -> MVar NodeQuery -> Int -> IO (Chan ITX)
toPeerFeed ctx hub port = do
  feed <- newChan
  forkIO $
    withSocket ctx Sub $ \sock -> do
      let addr = "tcp://127.0.0.1:" ++ show port
      print addr
      connect sock addr
      subscribe sock ""
      forever $ do
        bs <- fromStrict <$> receive sock
        case decode bs of
             TxAdvisory stx -> putMVar hub (AdviseTx stx)
             TxInclusion itx -> writeChan feed itx
  pure feed
