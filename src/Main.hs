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

import Options.Applicative

import System.Remote.Monitoring


main :: IO ()
main = do
  let path = strOption (long "config" <> short 'c' <> help "config json path")
             <|> pure "config.json"
      clientFlag = switch (long "testClient")
      args = (,) <$> path <*> clientFlag
      parse = info (helper <*> args) mempty

  (configPath, startClient) <- execParser parse

  -- forkServer "localhost" 18090

  config <- loadConfig configPath
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

    if startClient
       then postTxs $ sha3 ""
       else forever $ threadDelay 1000000

