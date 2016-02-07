module Network.BitTorrent.PWP where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as B

data PWP = KeepAlive
         | Choke
         | Unchoke
         | Interested
         | Uninterested
         | Have Word32
         | Bitfield B.ByteString
         | Request Word32 Word32 Word32
         | Piece Word32 Word32 B.ByteString
         | Cancel Word32 Word32 Word32
         deriving (Show)

instance Binary PWP where
  put KeepAlive =
    put (0 :: Word32)
  put Choke = do
    put (1 :: Word32)
    put (0 :: Word8)
  put Unchoke = do
    put (1 :: Word32)
    put (1 :: Word8)
  put Interested = do
    put (1 :: Word32)
    put (2 :: Word8)
  put Uninterested = do
    put (1 :: Word32)
    put (3 :: Word8)
  put (Have pieceId) = do
    put (5 :: Word32)
    put (4 :: Word8)
    put pieceId
  put (Bitfield field) = do
    put (fromIntegral $ 1 + B.length field :: Word32)
    put (5 :: Word8)
    putByteString field
  put (Request piece offset len) = do
    put (13 :: Word32)
    put (6 :: Word8)
    put piece
    put offset
    put len
  put (Piece piece offset d) = do
    put (fromIntegral $ 9 + B.length d :: Word32)
    put (7 :: Word8)
    put piece
    put offset
    putByteString d
  put (Cancel piece offset len) = do
    put (13 :: Word32)
    put (8 :: Word8)
    put piece
    put offset
    put len

  get = do
    len <- get :: Get Word32
    case len of
      0 -> return KeepAlive
      _ -> do
        messageId <- get :: Get Word8
        case messageId of
          0 -> return Choke
          1 -> return Unchoke
          2 -> return Interested
          3 -> return Uninterested
          4 -> Have <$> get
          5 -> Bitfield <$> getByteString (fromIntegral len - 1)
          6 -> Request <$> get <*> get <*> get
          7 -> Piece <$> get <*> get <*> getByteString (fromIntegral len - 9)
          8 -> Cancel <$> get <*> get <*> get
          _ -> fail "incorrect message!"
