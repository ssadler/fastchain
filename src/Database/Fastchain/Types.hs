{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Types where

import Data.Binary.Orphans
import qualified Data.ByteString.Base16 as B16

import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types hiding (Binary(..))

import GHC.Generics

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude

import System.ZMQ4


--------------------------------------------------------------------------------
-- Txid

newtype Txid = Txid ByteString
  deriving (Eq, Ord, Generic)

instance Binary Txid

instance FromJSON Txid where
  parseJSON val = Txid . fst . B16.decode . encodeUtf8 <$> parseJSON val

instance ToJSON Txid where
  toJSON (Txid bs) = toJSON $ decodeUtf8 $ B16.encode bs

instance ToField Txid where
  toField (Txid t) = toField t

instance FromField Txid where
  fromField f = fmap Txid . fromField f

instance Show Txid where
  show (Txid t) = show $ B16.encode t


--------------------------------------------------------------------------------
-- Transaction

data Transaction = Tx
  { _txid   :: Txid
  , _spends :: [Txid]
  } deriving (Eq, Show, Generic)

instance Binary Transaction

instance Ord Transaction where
  compare t u = _txid t `compare` _txid u

instance FromJSON Transaction 

instance ToJSON Transaction where
  toEncoding = genericToEncoding defaultOptions

instance ToRow Transaction where
  toRow (Tx a b) = [toField a, toField $ PGArray b]

instance FromRow Transaction where
  fromRow = Tx <$> field <*> (fromPGArray <$> field)

type STX = (UTCTime, Transaction)


--------------------------------------------------------------------------------
-- Node


type PeerId = PublicKey

type Peers = Map PeerId Server

type Backlog = Map UTCTime Transaction

type Server = MVar NodeQuery

data Node = Node
  { _config  :: Config
  , _pubSock :: Socket Pub
  , _router  :: MVar NodeQuery
  , _peers   :: Map PublicKey (Socket Sub)
  }


type SigMap = Map PublicKey Signature

data NodeQuery =
    PushTx Transaction
  | ConsiderTx [(STX,Signature)]

data BroadcastMessage =
    TxAdvisory STX Signature
  | TxInclusion STX Signature
  deriving (Generic)

instance Binary BroadcastMessage


-------------------------------------------------------------------------------- 
-- Config

data Config = Config
  { keyPair' :: KeyPair
  , peers' :: Map PublicKey String
  , dsn' :: String
  } deriving (Generic, Show)


instance ToJSON Config where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Config


