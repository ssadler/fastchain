{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Data.String

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Node
import Database.Fastchain.Schema
import Database.Fastchain.Types

import Network.HTTP.Simple

import System.IO


main :: IO ()
main = do

  setupLogging
    
  nodes@[n0,n1,n2] <- mapM createNode [0,1,2]
  mapM_ (forkIO . runNode) nodes

  let addPeer a b = push (_server a) $ AddPeer (_pubkey b) (_server b)

  mapM_ (addPeer n0) [n1,n2]
  mapM_ (addPeer n1) [n0,n2]
  mapM_ (addPeer n2) [n0,n1]

  threadDelay 1000000

  infoM "main" "Running client"
  runClient "http://localhost:3000/transactions"


createNode :: Int -> IO Node
createNode i = do
  let (p, s) = genKeyPairSeed i
      dsn = fromString $ "dbname=fastchain" ++ show i
  conn <- connectPostgreSQL dsn
  createSchema conn
  server <- newEmptyMVar
  peers <- newMVar mempty
  backlog <- newMVar mempty
  pure $ Node dsn (3000+i) server p s peers backlog


runClient :: String -> IO ()
runClient urlBase = go []
  where
    req val = let url = "POST " ++ urlBase
               in httpLBS $ setRequestBodyJSON val $ fromString url
    go spends = do
      let txBody = "{spends}" .% spends
          txid = decodeUtf8 $ sha3 $ toStrict $ encode txBody
          txVal = "{tx}" .% Tx txid spends
      res <- getResponseBody <$> req txVal
      threadDelay 1000000
      go [txid]


setupLogging :: IO ()
setupLogging = do
  let fmt = tfLogFormatter "%T" "$loggername \t$msg"
  h <- flip setFormatter fmt <$> streamHandler stderr DEBUG
  updateGlobalLogger "" $ setLevel DEBUG . setHandlers [h]

