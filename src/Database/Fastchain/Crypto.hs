{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Fastchain.Crypto where


import Crypto.Hash
import Crypto.Error (CryptoFailable(..))
import Crypto.Random
import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Binary
import Data.ByteArray as BA
import Data.ByteString as BS
import Data.ByteString.Base16 as B16
import qualified Data.Map.Strict as Map

import Database.Fastchain.Prelude


data PublicKey = PK Ed2.PublicKey
  deriving (Eq)

instance Show PublicKey where
  show (PK s) = "PK " ++ (show $ toTextKey s)

instance Ord PublicKey where
  compare a b = compare (show a) (show b)

instance ToJSON PublicKey where
  toJSON (PK k) = toJSONKey k

instance ToJSON a => ToJSON (Map PublicKey a) where
  toJSON = toJSON . Map.toList

instance FromJSON a => FromJSON (Map PublicKey a) where
  parseJSON = fmap Map.fromList . parseJSON

instance FromJSON PublicKey where
  parseJSON = fmap PK . fromJSONKey Ed2.publicKey

data SecretKey = SK Ed2.SecretKey

instance ToJSON SecretKey where
  toJSON (SK k) = toJSONKey k

instance FromJSON SecretKey where
  parseJSON = fmap SK . fromJSONKey Ed2.secretKey

instance Show SecretKey where
  show _ = "[SecretKey]"


data Signature = Sig Ed2.Signature
  deriving (Eq)

instance Show Signature where
  show (Sig s) = "Sig " ++ (Prelude.take 11 $ show $ toTextKey s) ++ "...\""

instance Binary Signature where
  put (Sig sig) = put $ BA.unpack sig
  get = do vfail <- Ed2.signature <$> (get :: Get ByteString)
           Sig <$> fromKey vfail


type KeyPair = (PublicKey, SecretKey)


toTextKey :: ByteArrayAccess a => a -> Text
toTextKey = decodeUtf8 . B16.encode . BA.pack . BA.unpack


toJSONKey :: ByteArrayAccess a => a -> Value
toJSONKey = toJSON . toTextKey


fromJSONKey :: (ByteString -> CryptoFailable a) -> Value -> Parser a
fromJSONKey toKey v = do
    t <- parseJSON v
    let (d,_) = B16.decode $ encodeUtf8 t
    fromKey $ toKey d


fromKey :: Monad m => CryptoFailable a -> m a
fromKey (CryptoPassed a) = pure a
fromKey (CryptoFailed e) = fail $ show e


--------------------------------------------------------------------------------
-- Crypto Function wrappers

sign :: KeyPair -> ByteString -> Signature
sign ((PK pk),(SK sk)) = Sig . Ed2.sign sk pk


verify :: ByteArrayAccess ba => PublicKey -> ba -> Signature -> Bool
verify (PK pk) ba (Sig sig) = Ed2.verify pk ba sig


genKeyPair :: IO KeyPair
genKeyPair = _genKeyPair <$> getSystemDRG


genKeyPairSeed :: Int -> KeyPair
genKeyPairSeed = _genKeyPair . drgNewSeed . seedFromInteger . fromIntegral


_genKeyPair :: DRG t => t -> KeyPair
_genKeyPair drg = 
  let (bs, _) = randomBytesGenerate 32 drg
      (CryptoPassed sk) = Ed2.secretKey (bs::ByteString)
   in (PK $ Ed2.toPublic sk, SK sk)


sha3 :: ByteString -> ByteString
sha3 bs = BS.take 16 $ B16.encode $ BA.pack $ BA.unpack (hash bs :: Digest SHA3_256)
