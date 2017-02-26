{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Fastchain.App
import Database.Fastchain.Backlog
import Database.Fastchain.Config
import Database.Fastchain.Crypto
import Database.Fastchain.Hub
import Database.Fastchain.Http
import Database.Fastchain.Node
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types
import Database.Fastchain.Zip

import Options.Applicative

import System.IO


main :: IO ()
main = do
  let path = strOption (long "config" <> short 'c' <> help "config json path")
             <|> pure "config.json"
      clientFlag = switch (long "testClient")
      args = (,) <$> path <*> clientFlag
      parse = info (helper <*> args) mempty

  (configPath, startClient) <- execParser parse
  setupLogging
  node <- loadConfig configPath >>= makeNode
  -- when startClient $ (forkIO $ testClient node 1000000) *> pure ()
  db_ node dbSetup
  runNode node


runNode :: Node -> IO ()
runNode node = do
  withHub node $ \feeds -> do
    forkIO $ runBacklog node
    forkIO $ runHttp node
    runZipIO feeds $ onVotedTx node


setupLogging :: IO ()
setupLogging = do
  let fmt = tfLogFormatter "%T" "$loggername \t$msg"
  h <- flip setFormatter fmt <$> streamHandler stderr DEBUG
  updateGlobalLogger "" $ setLevel DEBUG . setHandlers [h]
