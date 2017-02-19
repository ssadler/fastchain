module Database.Fastchain.Prelude
  ( module SL
  , module ALL
  , ByteString
  , Text
  , lift
  , (<>)
  -- local
  , c8Pack
  , request
  , push
  , Effectful
  , eff
  , eff1
  , runEffects
  , forkIO
  ) where

import Control.Concurrent as ALL hiding (forkIO)
import Control.Monad as ALL
import Control.Monad.IO.Class as ALL
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader as ALL

import Data.Aeson.Quick as ALL hiding (encode, decode, json)
import Data.ByteString.Lazy as ALL (toStrict, fromStrict)
import Data.ByteString as BS
import Data.ByteString.Char8 as C8
import Data.Int as ALL
import Data.List as ALL (nub, sortOn)
import Data.Map.Strict as ALL (Map, fromList, toList)
import Data.Maybe as ALL
import Data.Monoid
import Data.Set as ALL (Set)
import Data.Text
import Data.Text.Encoding as ALL (encodeUtf8, decodeUtf8)
import Data.Time.Clock as ALL

import Database.PostgreSQL.Simple as ALL hiding (Binary(..), connect)

import Lens.Micro as ALL
import Lens.Micro.Platform as ALL ()

import SlaveThread

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


forkIO :: IO a -> IO ThreadId
forkIO = fork


--------------------------------------------------------------------------------
-- Effect helpers

type Effectful a m = ReaderT (a m) m

eff :: Monad m => (a -> ReaderT a m b) -> ReaderT a m b
eff f = ask >>= f

eff1 :: Monad m => (a -> c -> ReaderT a m b) -> c -> ReaderT a m b
eff1 f a = ask >>= flip f a

runEffects :: Effectful a m b -> a m -> m b
runEffects = runReaderT


--------------------------------------------------------------------------------
-- Data helpers

c8Pack :: String -> ByteString
c8Pack = C8.pack
