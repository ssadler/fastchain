{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Data.String

import Database.Fastchain.Config
import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Node
import Database.Fastchain.Schema
import Database.Fastchain.Types

import Network.HTTP.Simple

import System.IO
import System.ZMQ4


main :: IO ()
main = do
  setupLogging
  config <- loadConfig
  withNode config runNode


withNode :: Config -> (Node -> IO a) -> IO a
withNode c@(Config keyPair peers dsn) act = do
  conn <- connectPostgreSQL $ fromString dsn
  createSchema conn
  withContext $ \ctx -> do
    withSocket ctx Pub $ \pubSock -> do
      withPeers ctx peers $ \socks -> do
        server <- newEmptyMVar
        let node = Node c pubSock server socks
        act node


withPeers :: Context -> Map PublicKey String -> 
          (Map PublicKey (Socket Sub) -> IO a) -> IO a
withPeers ctx peerAddrMap act = do
  let eachPeer [] socks = act $ fromList socks
      eachPeer ((pk,addr):xs) socks = do
        withSocket ctx Sub $ \s ->
          eachPeer xs ((pk,s):socks)
   in eachPeer (toList peerAddrMap) []


setupLogging :: IO ()
setupLogging = do
  let fmt = tfLogFormatter "%T" "$loggername \t$msg"
  h <- flip setFormatter fmt <$> streamHandler stderr DEBUG
  updateGlobalLogger "" $ setLevel DEBUG . setHandlers [h]


--runClient :: String -> IO ()
--runClient urlBase = go []
--  where
--    req val = let url = "POST " ++ urlBase
--               in httpLBS $ setRequestBodyJSON val $ fromString url
--    go spends = do
--      let txBody = "{spends}" .% spends
--          txid = decodeUtf8 $ sha3 $ toStrict $ encode txBody
--          txVal = "{tx}" .% Tx txid spends
--      res <- getResponseBody <$> req txVal
--      threadDelay 1000000
--      go [txid]
