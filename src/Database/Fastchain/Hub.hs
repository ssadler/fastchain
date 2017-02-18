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
withHub (Config (me,_) peers _) act = do
  hub <- newEmptyMVar
  withContext $ \ctx -> do
    withSocket ctx Pub $ \pub -> do
      let addr = "inproc://hello"
          peersAndMe = (me,addr) : toList peers
      bind pub addr
      feeds <- traverseOf (each . _2) (toPeerFeed ctx hub) peersAndMe
      let broadcast = send' pub [] . encode
      act $ HubInterface hub feeds broadcast


toPeerFeed :: Context -> MVar NodeQuery -> String -> IO (Chan ITX)
toPeerFeed ctx hub addr = do
  feed <- newChan
  forkIO $
    withSocket ctx Sub $ \sock -> do
      connect sock addr
      subscribe sock ""
      forever $ do
        bs <- fromStrict <$> receive sock
        case decode bs of
             TxAdvisory stx -> putMVar hub (AdviseTx stx)
             TxInclusion itx -> writeChan feed itx
  pure feed
