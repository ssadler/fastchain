{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Fastchain.Schema where

import qualified Data.Set as Set

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Database.Fastchain.Prelude
import Database.Fastchain.Types


dbConnect :: Node -> IO Connection
dbConnect = connectPostgreSQL . _dsn


db :: Node -> (Connection -> a -> IO b) -> a -> IO b
db node q a0 = do
  conn <- dbConnect node
  q conn a0 <* close conn


db_ :: Node -> (Connection -> IO b) -> IO b
db_ node q = do
  conn <- dbConnect node
  q conn <* close conn


createSchema :: Connection -> IO ()
createSchema conn = do
  execute_ conn "drop table if exists transactions"
  execute_ conn "create table transactions (\
                  \ seq serial, \
                  \ ts timestamp with time zone, \
                  \ txid varchar(64) not null unique, \
                  \ spends varchar(64)[] not null )"
  pure ()


insertTx :: Connection -> Transaction -> IO Int64
insertTx conn = execute conn "insert into transactions (txid,ts,spends) \
                               \ values (?,?,?)" 


insertTxs :: Connection -> [STX] -> IO Int64
insertTxs conn = executeMany conn sql . map (\(ts, tx) -> Only ts :. tx)
  where
    sql = "insert into transactions (ts,txid,spends) values (?, ?, ?) \
            \ on conflict (txid) do nothing"


selectTxsFrom :: Connection -> Int -> IO [(Integer, STX)]
selectTxsFrom conn from = map unRow <$> query conn sql (from, from+1000)
  where
    unRow (Only s :. Only t :. tx) = (s,(t,tx))
    sql = "select seq, ts, txid, spends from transactions \
            \ where seq >= ? and seq < ? order by seq asc"


getSpentOf :: Connection -> [Txid] -> IO (Set Txid)
getSpentOf conn txids =
  Set.fromList <$> fmap fromOnly <$> query conn sql args
  where args = (PGArray txids, PGArray txids)
        sql = "select s from \
    \ ( select unnest(spends) as s from transactions \
    \   where (spends::text[]) && ? ) as matches \
    \ where s = ANY (?)"
