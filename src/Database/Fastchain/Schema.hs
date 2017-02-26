{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Fastchain.Schema where

import qualified Data.ByteString.Char8 as C8

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Database.Fastchain.Prelude
import Database.Fastchain.Types



dbConnect :: Node -> IO Connection
dbConnect = connectPostgreSQL . C8.pack . _dsn . _config


db :: Node -> (Connection -> a -> IO b) -> a -> IO b
db node q a0 = do
  conn <- dbConnect node
  q conn a0 <* close conn


db_ :: Node -> (Connection -> IO b) -> IO b
db_ node q = do
  conn <- dbConnect node
  q conn <* close conn


dbSetup :: Connection -> IO ()
dbSetup conn = do
  C8.readFile "system.sql" >>= execute_ conn . Query
  pure ()


