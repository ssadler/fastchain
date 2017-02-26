{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Hub where

import Data.Binary

import qualified Data.ByteString as BS

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Node
import Database.Fastchain.Types

import System.ZMQ4


type Feeds = [(PublicKey, Socket Sub)]


-- Hub
-- Create subscription to each peer
-- Create mvar to push broadcast over Pub
-- For each peer, should create subscription to 
withHub :: Node -> (Feeds -> IO a) -> IO a
withHub node act = do
  let (Config (me,_) peers port _) = _config node
      onAdvisory = onAdviseTx node
  withContext $ \ctx -> do
    withBroadcast node ctx $ do
      let peersAndMe = nub $ (me,port) : peers
      traverseOf (each . _2) (forkCopyAdvisory ctx onAdvisory) peersAndMe
      withSubscribers ctx peersAndMe $ \subs ->
        act subs


withBroadcast :: Node -> Context -> IO a -> IO a
withBroadcast node ctx act = do
  let out = _broadcastOut node
  withSocket ctx Pub $ \pub -> do
    bind pub $ pubAddr $ _port $ _config node
    forkIO $ forever $ do
      takeMVar out >>= send' pub [] . encode
    act


withSubscribers :: Context -> [(PublicKey, Int)] ->
                   ([(PublicKey, Socket Sub)] -> IO a) -> IO a
withSubscribers ctx peers act = mk peers []
  where mk [] socks = act socks
        mk ((pk,port):xs) socks =
          withSocket ctx Sub $ \sock -> do
            connect sock $ pubAddr port
            subscribe sock "\1"
            mk xs ((pk,sock):socks)


forkCopyAdvisory :: Context -> (STX -> IO ()) -> Int -> IO ThreadId
forkCopyAdvisory ctx onAdvisory port = forkIO $
  withSocket ctx Sub $ \sock -> do
    connect sock $ pubAddr port
    subscribe sock "\0"
    forever $ do
      bs <- fromStrict . BS.drop 1 <$> receive sock
      onAdvisory $ decode bs


pubAddr :: Int -> String
pubAddr port = "tcp://127.0.0.1:" ++ show port
