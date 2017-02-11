{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Types where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude


--------------------------------------------------------------------------------
-- Transaction


type Txid = Text

data Transaction = Tx Txid [Txid]
  deriving (Eq, Show)

instance FromJSON Transaction where
  parseJSON v = uncurry Tx <$> extract "{txid,spends}" v

instance ToJSON Transaction where
  toJSON (Tx txid spends) = "{txid,spends}" .% (txid, spends)

instance ToRow Transaction where
  toRow (Tx a b) = [toField a, toField $ PGArray b]

instance FromRow Transaction where
  fromRow = Tx <$> field <*> (fromPGArray <$> field)


--------------------------------------------------------------------------------
-- Node


type PeerId = PublicKey

type Peers = Map PeerId Server

type Backlog = Map UTCTime Transaction

type Server = MVar NodeQuery

data Node = Node
  { _dsn :: ByteString
  , _httpPort :: Int
  , _server :: Server
  , _pubkey :: PublicKey
  , _secret :: SecretKey
  , _peers :: MVar Peers
  , _backlog :: MVar Backlog
  }


type SigMap = Map PublicKey Signature

data NodeQuery =
    AddPeer PeerId Server
  | PostTx Transaction
  | PlzTimestamp Txid UTCTime (MVar (Maybe Signature))
  | HasTimestamp Transaction UTCTime SigMap
  
