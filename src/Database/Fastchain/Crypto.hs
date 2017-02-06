{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Fastchain.Crypto where


import Crypto.Error (CryptoFailable(..))
import Crypto.Random
import qualified Crypto.PubKey.Ed25519 as Ed2

import Data.ByteArray
import Data.ByteString (ByteString)


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
