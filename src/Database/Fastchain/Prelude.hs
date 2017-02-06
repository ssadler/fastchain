module Database.Fastchain.Prelude
  ( module AEQ
  , module BSL
  , module CM
  , module CMR
  , module DPS
  , module DTC
  , module DTE
  , module Data.Maybe
  , module MIC
  , ByteString
  , Map
  , MVar
  , Text
  , lift
  , (<>)
  -- local
  , sha3
  , request
  , push
  ) where

import Control.Concurrent
import Control.Monad as CM
import Control.Monad.IO.Class as MIC
import Control.Monad.Trans.Reader as CMR
import Control.Monad.Trans.Class

import Crypto.Hash

import Data.Aeson.Quick as AEQ hiding (json)
import Data.ByteString.Lazy as BSL (toStrict, fromStrict)
import Data.ByteArray as BA
import Data.ByteString as BS
import Data.ByteString.Base16 as B16
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text
import Data.Text.Encoding as DTE (encodeUtf8, decodeUtf8)
import Data.Time.Clock as DTC

import Database.PostgreSQL.Simple as DPS hiding (connect)


sha3 :: BS.ByteString -> BS.ByteString
sha3 bs = B16.encode $ BS.pack $ BA.unpack (hash bs :: Digest SHA3_256)


request :: MonadIO m => MVar a -> (MVar b -> a) -> m b
request node part = liftIO $ do
  out <- newEmptyMVar
  putMVar node (part out)
  takeMVar out


push :: MonadIO m => MVar a -> a -> m ()
push n = liftIO . putMVar n

