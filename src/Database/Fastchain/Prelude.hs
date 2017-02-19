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
  , eff2
  , runEffects
  , Logs(..)
  , timed
  ) where

import Control.Concurrent as ALL
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

import Database.PostgreSQL.Simple as ALL hiding (Binary(..), In(..), connect)

import Lens.Micro as ALL
import Lens.Micro.Platform as ALL ()

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


--------------------------------------------------------------------------------
-- Effect helpers

type Effectful a m = ReaderT (a m) m

eff :: Monad m => (a -> ReaderT a m b) -> ReaderT a m b
eff f = ask >>= f

eff1 :: Monad m => (a -> c -> ReaderT a m b) -> c -> ReaderT a m b
eff1 f a = ask >>= flip f a

eff2 :: Monad m => (a -> b -> c -> ReaderT a m d) -> b -> c -> ReaderT a m d
eff2 f a b = do
  e <- ask
  f e a b

runEffects :: Effectful a m b -> a m -> m b
runEffects = runReaderT


--------------------------------------------------------------------------------
-- Logging 

class Monad m => Logs m where
  logE :: Priority -> String -> m ()
  getTime' :: m UTCTime


timed :: Logs m => Priority -> String -> m a -> m a
timed p s act = do
  t <- getTime'
  o <- act
  diff <- (round . (*(-1000))) . diffUTCTime t <$> getTime'
  logE p (s ++ " in " ++ show diff ++ "ms")
  pure o



--------------------------------------------------------------------------------
-- Data helpers

c8Pack :: String -> ByteString
c8Pack = C8.pack
