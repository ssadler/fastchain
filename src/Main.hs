{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Data.String

import Database.Fastchain.Prelude
import Database.Fastchain.Http
import Database.Fastchain.Schema
import Database.Fastchain.Types

import Network.HTTP.Simple


data Node = Node
  { _httpPort :: Int
  , _dsn :: ByteString
  , _pub :: PublicKey
  , _secret :: SecretKey
  }


main :: IO ()
main = do
  let [(p0,s0),(p1,s1),(p2,s2)] = genKeyPairSeed <$> [3000,3001,3002]
      node0 = Node 3000 "dbname=fastchain0" p0
      node1 = Node 3001 "dbname=fastchain1" p1
      node2 = Node 3002 "dbname=fastchain2" p2

  n0 <- forkIO $ runNode (node0 s0) [node1 undefined, node2 undefined]
  n1 <- forkIO $ runNode (node1 s1) [node0 undefined, node2 undefined]
  n2 <- forkIO $ runNode (node2 s2) [node0 undefined, node1 undefined]

  threadDelay 1000000
  runClient "http://localhost:3000/transactions"


runNode :: Node -> [Node] -> IO ()
runNode node@(Node port dsn pub secret) upstreams = do
  conn <- connectPostgreSQL dsn
  createSchema conn
  mapM_ (forkIO . replicateTxs node) upstreams
  runServer conn port


replicateTxs :: Node -> Node -> IO ()
replicateTxs (Node _ dsn _ _) (Node port _ _ _) = do

  conn <- connectPostgreSQL dsn
  let go offset = do
          let uri = "http://localhost:" ++ show port ++
                        "/transactions?offset=" ++ show offset
              req = fromString uri
          res <- getResponseBody <$> httpJSON req
          let (mnext,txs) = res .! jsonTxsFrom
          inserted <- insertTxs conn txs
          threadDelay 1000 -- 000
          go $ maybe offset id mnext
  go (0::Int)


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
      --threadDelay 1000 -- 000
      go [txid]

