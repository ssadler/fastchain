module Database.Fastchain.Prelude
  ( module AEQ
  , module BSL
  , module CM
  , module DPS
  , module DTC
  , module DTE
  , module Data.Maybe
  , module LM
  , module LMP
  , module Map
  , module MIC
  , module SL
  , ByteString
  , MVar
  , Text
  , lift
  , (<>)
  -- local
  , request
  , push
  ) where

import Control.Concurrent
import Control.Monad as CM
import Control.Monad.IO.Class as MIC
import Control.Monad.Trans.Class

import Data.Aeson.Quick as AEQ hiding (json)
import Data.ByteString.Lazy as BSL (toStrict, fromStrict)
import Data.ByteString as BS
import Data.Map.Strict as Map (Map, fromList, toList)
import Data.Maybe
import Data.Monoid
import Data.Text
import Data.Text.Encoding as DTE (encodeUtf8, decodeUtf8)
import Data.Time.Clock as DTC

import Database.PostgreSQL.Simple as DPS hiding (connect)

import Lens.Micro as LM
import Lens.Micro.Platform as LMP ()

import System.Log.Logger as SL
import System.Log.Handler as SL (setFormatter)
import System.Log.Handler.Simple as SL
import System.Log.Formatter as SL



--------------------------------------------------------------------------------
-- Actor interface

request :: MonadIO m => MVar a -> (MVar b -> a) -> m b
request node part = liftIO $ do
  out <- newEmptyMVar
  putMVar node (part out)
  takeMVar out


push :: MonadIO m => MVar a -> a -> m ()
push n = liftIO . putMVar n
