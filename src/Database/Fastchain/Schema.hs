{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Schema where


import qualified Data.ByteString as BS
import Data.Text (Text)
import Database.PostgreSQL.Simple


createSchema :: IO ()
createSchema = do
  conn <- connectPostgreSQL "dbname=fastchain"
  execute_ conn "drop table if exists transactions"
  execute_ conn "create table transactions (\
                  \ seq serial, \
                  \ txid varchar(64) not null, \
                  \ voted boolean not null, \
                  \ valid boolean not null )"
  execute_ conn "insert into transactions (txid,voted,valid) values ('a', false, false)"
  results <- query_ conn "select * from transactions" :: IO [(Int, Text, Bool, Bool)]
  print results
