module Network.IP where

import qualified Data.Binary     as Binary
import           Data.List (intercalate)
import           Network.IP.Addr (IP4, ip4ToOctetList)

stringip :: IP4 -> String
stringip = intercalate "." . map show . ip4ToOctetList

decimalip :: (Num a) => IP4 -> a
decimalip address = sum . zipWith (curry shift) [0..] $ ip4ToOctetList address
  where
    shift :: (Num a) => (Int, Binary.Word8) -> a
    shift (x,y) = 2^(8*x)*fromIntegral y
