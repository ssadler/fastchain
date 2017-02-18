{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Fastchain.Backlog
import Database.Fastchain.Config
import Database.Fastchain.Crypto
import Database.Fastchain.Hub
import Database.Fastchain.Node
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types
import Database.Fastchain.Zip


main :: IO ()
main = do
  config <- loadConfig
  withHub config $ \(HubInterface hub feeds broadcast) -> do
    node <- makeNode config hub
    db_ node createSchema
    forkIO $ runZipIO hub feeds
    forkIO $ runBacklog node
    forkIO $ runNode node broadcast

    let postTxs txid = do
          putMVar (_hub node) $ ClientTx $ Tx (Txid txid) []
          threadDelay 1000000
          postTxs $ sha3 txid

    postTxs $ sha3 ""


--setupLogging :: IO ()
--setupLogging = do
--  let fmt = tfLogFormatter "%T" "$loggername \t$msg"
--  h <- flip setFormatter fmt <$> streamHandler stderr DEBUG
--  updateGlobalLogger "" $ setLevel DEBUG . setHandlers [h]


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
