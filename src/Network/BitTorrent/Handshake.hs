module Network.BitTorrent.Handshake where

import           Data.Binary                (Word64, Word8)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString            (ByteString, hPut)
import qualified Data.ByteString.Lazy.Char8 as BL
import           System.IO                  (Handle)

data Handshake = Handshake {
  len      :: Maybe Int
, name     :: Maybe ByteString
, reserved :: Maybe ByteString
, infoHash :: Maybe ByteString
, peerId   :: Maybe ByteString
} deriving (Show)

-- | createHandshake to send.
createHandshake :: ByteString -> ByteString -> ByteString
createHandshake hash myId = BL.toStrict $ BL.concat [a,b,c,d,e]
  where
    a = runPut $ putWord8 (19 :: Word8)
    b = runPut $ putByteString "BitTorrent protocol"
    c = runPut $ putWord64be (0 :: Word64)
    d = runPut $ putByteString hash
    e = runPut $ putByteString myId

--            Length | Protocol name | Reserved | Info Hash | Peer Id
handshakeLen =   1   +      19        +    8     +    20     +   20
handshakeLen :: (Num a) => a

-- | parseHandshake parses a handshake packages.
parseHandshake :: BL.ByteString -> Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Handshake)
parseHandshake = runGetOrFail f
  where
    f = do
      a <- getWord8
      b <- getByteString (fromIntegral a)
      c <- getByteString 8
      d <- getByteString 20
      e <- getByteString 20
      return Handshake {
        len      = Just $ fromIntegral a
      , name     = Just b
      , reserved = Just c
      , infoHash = Just d
      , peerId   = Just e
      }

-- | writeHandshake to handle.
writeHandshake :: Handle -> ByteString -> ByteString -> IO ()
writeHandshake h hash myId = hPut h $ createHandshake hash myId
