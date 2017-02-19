{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Hub where

import Data.Binary

import qualified Data.ByteString as BS

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types

import System.ZMQ4

import Debug.Trace

data HubInterface = HubInterface 
  { _hhub :: MVar NodeQuery
  , _peers :: [(PublicKey, Socket Sub)]
  , _broadcast :: BroadcastMessage -> IO ()
  }


withHub :: Config -> (HubInterface -> IO a) -> IO a
withHub (Config (me,_) peers port _) act = do
  hub <- newEmptyMVar
  withContext $ \ctx -> do
    withSocket ctx Pub $ \pub -> do
      let peersAndMe = nub $ (me,port) : peers
      bind pub $ pubAddr port
      traverseOf (each . _2) (forkCopyAdvisory ctx hub) peersAndMe
      let broadcast = send' pub [] . encode
      withSubscribers ctx peersAndMe $ \subs ->
        act $ HubInterface hub subs broadcast


withSubscribers :: Context -> [(PublicKey, Int)] ->
                   ([(PublicKey, Socket Sub)] -> IO a) -> IO a
withSubscribers ctx peers act = mk peers []
  where mk [] socks = act socks
        mk ((pk,port):xs) socks =
          withSocket ctx Sub $ \sock -> do
            connect sock $ pubAddr port
            subscribe sock "\1"
            mk xs ((pk,sock):socks)


forkCopyAdvisory :: Context -> MVar NodeQuery -> Int -> IO ThreadId
forkCopyAdvisory ctx hub port = forkIO $
  withSocket ctx Sub $ \sock -> do
    connect sock $ pubAddr port
    subscribe sock "\0"
    forever $ do
      bs <- fromStrict . BS.drop 1 <$> receive sock
      putMVar hub $ AdviseTx $ decode bs


pubAddr :: Int -> String
pubAddr port = "tcp://127.0.0.1:" ++ show port
