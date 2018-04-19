{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Fastchain.App
import Database.Fastchain.Backlog
import Database.Fastchain.Config
import Database.Fastchain.Hub
import Database.Fastchain.Http
import Database.Fastchain.Node
import Database.Fastchain.Monitors
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

  (configPath, _) <- execParser parse
  setupLogging

  conf <- loadConfig configPath
  ekg <- initMonitors $ 18500 + mod (_port conf) 100
  node <- makeNode conf ekg
  db_ node dbSetup
  runNode node


runNode :: Node -> IO ()
runNode node = do
  withHub node $ \feeds -> do
    forkIO $ runBacklog node
    forkIO $ runHttp node
    forkIO $ runApp node
    runZipIO feeds $ onVotedTx node


setupLogging :: IO ()
setupLogging = do
  let fmt = tfLogFormatter "%T" "$loggername \t$msg"
  h <- flip setFormatter fmt <$> streamHandler stderr DEBUG
  updateGlobalLogger "" $ setLevel DEBUG . setHandlers [h]
