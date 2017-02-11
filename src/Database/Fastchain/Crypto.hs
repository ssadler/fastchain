{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Fastchain.Crypto where


import Crypto.Hash
import Crypto.Error (CryptoFailable(..))
import Crypto.Random
import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.ByteArray as BA
import Data.ByteString as BS (ByteString, take)
import Data.ByteString.Base16 as B16


class IsKey k a


data PublicKey = PK Ed2.PublicKey
  deriving (Eq, Show)

instance Ord PublicKey where
  compare a b = compare (show a) (show b)


data SecretKey = SK Ed2.SecretKey


data Signature = Sig Ed2.Signature
  deriving (Show)


sign :: SecretKey -> PublicKey -> ByteString -> Signature
sign (SK sk) (PK pk) = Sig . Ed2.sign sk pk


verify :: ByteArrayAccess ba => PublicKey -> ba -> Signature -> Bool
verify (PK pk) ba (Sig sig) = Ed2.verify pk ba sig


genKeyPair :: IO (PublicKey, SecretKey)
genKeyPair = _genKeyPair <$> getSystemDRG


genKeyPairSeed :: Int -> (PublicKey, SecretKey)
genKeyPairSeed = _genKeyPair . drgNewSeed . seedFromInteger . fromIntegral


_genKeyPair :: DRG t => t -> (PublicKey, SecretKey)
_genKeyPair drg = 
  let (bs, _) = randomBytesGenerate 32 drg
      (CryptoPassed sk) = Ed2.secretKey (bs::ByteString)
   in (PK $ Ed2.toPublic sk, SK sk)


sha3 :: ByteString -> ByteString
sha3 bs = BS.take 16 $ B16.encode $ pack $ BA.unpack (hash bs :: Digest SHA3_256)
