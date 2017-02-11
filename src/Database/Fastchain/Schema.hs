{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Fastchain.Schema where

import Data.Int

import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Database.Fastchain.Types


dbConnect :: Node -> IO Connection
dbConnect = connectPostgreSQL . _dsn


db :: Node -> (Connection -> a -> IO b) -> a -> IO b
db node q a0 = do
  conn <- dbConnect node
  q conn a0


createSchema :: Connection -> IO ()
createSchema conn = do
  execute_ conn "drop table if exists transactions"
  execute_ conn "create table transactions (\
                  \ seq serial, \
                  \ txid varchar(64) not null unique, \
                  \ ts timestamp with time zone, \
                  \ spends varchar(64)[] not null )"
  pure ()


insertTx :: Connection -> Transaction -> IO Int64
insertTx conn = execute conn "insert into transactions (txid,ts,spends) \
                               \ values (?,?,?)" 


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


getSpentOf :: Connection -> [Txid] -> IO [Txid]
getSpentOf conn = fmap (fmap fromOnly) . query conn sql . Only . PGArray
  where sql = "select s from \
    \ ( select unnest(spends) as s from transactions \
    \   where (spends::text[]) && ? \
    \ ) as matches \
    \ where s IN ?"

