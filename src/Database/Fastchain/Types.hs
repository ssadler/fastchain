{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Types where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import Database.Fastchain.Prelude



data Transaction = Tx Text [Text]

instance FromJSON Transaction where
  parseJSON v = uncurry Tx <$> extract "{txid,spends}" v

instance ToJSON Transaction where
  toJSON (Tx txid spends) = "{txid,spends}" .% (txid, spends)

instance ToRow Transaction where
  toRow (Tx a b) = [toField a, toField $ PGArray b]

instance FromRow Transaction where
  fromRow = Tx <$> field <*> (fromPGArray <$> field)
