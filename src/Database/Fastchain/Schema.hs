{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Fastchain.Schema where

import qualified Data.ByteString.Char8 as C8
import Data.Pool

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Database.Fastchain.Types


dbPool :: Config -> IO (Pool Connection)
dbPool conf = createPool (dbConnect conf) close 1 1 10
  where dbConnect = connectPostgreSQL . C8.pack . _dsn


db :: Node -> (Connection -> a -> IO b) -> a -> IO b
db node q = withResource (_dbPool node) . flip q


db_ :: Node -> (Connection -> IO b) -> IO b
db_ node = withResource (_dbPool node)


dbSetup :: Connection -> IO ()
dbSetup conn = do
  C8.readFile "system.sql" >>= execute_ conn . Query
  pure ()


