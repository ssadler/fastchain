{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Data.String

import Database.Fastchain.Advisory
import Database.Fastchain.Config
import Database.Fastchain.Crypto
import Database.Fastchain.Hub
import Database.Fastchain.Prelude
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
