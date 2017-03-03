{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Fastchain.Types where

import Data.Aeson.Types
import Data.Binary.Orphans

import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import GHC.Generics

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude

--------------------------------------------------------------------------------
-- App


type SQL = Text
type Id = Text


data Command =
    CreateApp { _sql :: SQL, _name :: Text }
  | Call { _proc :: Text, _body :: Value }
  deriving (Eq, Generic, Show)

instance Binary Command

cmdJsonOpts :: Options
cmdJsonOpts = defaultOptions { sumEncoding = TaggedObject "op" "data"
                             , fieldLabelModifier = drop 1
                             }

instance FromJSON Command where
  parseJSON = genericParseJSON cmdJsonOpts

instance ToJSON Command where
  toJSON = genericToJSON cmdJsonOpts


--------------------------------------------------------------------------------
-- Transaction

data Transaction = Tx
  { _txid :: Text
  , _cmd :: Command
  , _clientTime :: Text
  } deriving (Eq, Show, Generic)

instance Binary Transaction

instance Ord Transaction where
  compare t u = _txid t `compare` _txid u

instance FromJSON Transaction where
  parseJSON val =
    Tx <$> extract "{id}" val
       <*> parseJSON val
       <*> extract "{ts}" val

instance ToJSON Transaction where
  toJSON (Tx txid cmd t) =
    build "{id,ts}" (toJSON cmd) (txid,t)

instance ToRow Transaction where
  toRow (Tx a b t) = [toField a, toField $ toJSON b, toField t]

instance FromRow Transaction where
  fromRow = Tx <$> field <*> (toCommand <$> field) <*> field
    where toCommand = fromJust . decode

type STX = (UTCTime, Transaction)
type ITX = (STX, Signature)


--------------------------------------------------------------------------------
-- Node

type Backlog = Map UTCTime Transaction

data Node = Node
  { _config  :: Config
  , _backlog :: MVar Backlog
  , _broadcast :: BroadcastMessage -> IO ()
  , _broadcastOut :: MVar BroadcastMessage
  }


type SigMap = [(PublicKey, Signature)]


data BroadcastMessage =
    TxAdvisory STX
  | TxInclusion ITX
  deriving (Generic)

instance Binary BroadcastMessage


-------------------------------------------------------------------------------- 
-- Config

data Config = Config
  { _keyPair :: KeyPair
  , _peers :: [(PublicKey,Int)]
  , _port :: Int
  , _dsn :: String
  } deriving (Generic, Show)


instance ToJSON Config where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Config


--------------------------------------------------------------------------------
-- Orphans

instance Show (Chan a) where
  show _ = "Chan ?"
