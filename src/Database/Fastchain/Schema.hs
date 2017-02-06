{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Fastchain.Schema where

import Data.Int

import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Database.Fastchain.Prelude
import Database.Fastchain.Types


dbConnect :: RunNode Connection
dbConnect = do dsn <- _dsn <$> ask
               lift $ connectPostgreSQL dsn


db :: (Connection -> a -> IO b) -> a -> RunNode b
db q a0 = do
  conn <- dbConnect
  lift $ q conn a0


createSchema :: Connection -> IO ()
createSchema conn = do
  execute_ conn "drop table if exists transactions"
  execute_ conn "create table transactions (\
                  \ seq serial, \
                  \ txid varchar(64) not null unique, \
                  \ spends varchar(64)[] not null, \
                  \ voted boolean not null default false, \
                  \ parent bigint )"
  pure ()


insertTx :: Connection -> Transaction -> IO Int64
insertTx conn = execute conn "insert into transactions (txid,spends) \
                               \ values (?, ?)" 


insertTxs :: Connection -> [Transaction] -> IO Int64
insertTxs conn = executeMany conn sql
  where
    sql = "insert into transactions (txid,spends) values (?, ?) \
            \ on conflict (txid) do nothing"


selectTxsFrom :: Connection -> Int -> IO [Only Integer :. Transaction]
selectTxsFrom conn from = query conn sql (from, from+1000)
  where
    sql = "select seq, txid, spends from transactions \
            \ where seq >= ? and seq < ? order by seq asc"


getSpend :: Connection -> BS.ByteString -> IO [Only BS.ByteString]
getSpend conn = query conn sql . Only . PGArray . (:[])
  where sql = "select txid from transactions \
                \ where spends @> ? and parent is not null \
                \ limit 1"
