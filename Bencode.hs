-- Used by bkey. Don't know what this does yet.
{-# LANGUAGE RankNTypes #-}

module Bencode where
-- module Bencode (BValue, bstring, bnumber, blist, bkey, decode) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as B
import           Lens.Family2
import Text.Printf

-- BValue is all possible values a bencoded string can represent.
data BValue = String ByteString
            | Number Integer
            | List [BValue]
            | Dictionary [(ByteString, BValue)] deriving (Show)

decode :: ByteString -> Either String BValue
decode = P.parseOnly value

string' :: P.Parser BValue
string' = do
  n <- P.decimal
  void $ P.char ':'
  String `fmap` P.take n

number :: P.Parser BValue
number = do
  void $ P.char 'i'
  n <- P.decimal
  void $ P.char 'e'
  return $ Number n

list :: P.Parser BValue
list = do
  void $ P.char 'l'
  xs <- P.many' value
  void $ P.char 'e'
  return $ List xs

dictionary :: P.Parser BValue
dictionary = do
  void $ P.char 'd'
  pairs <- P.many' ((,) `fmap` string' <*> value)
  void $ P.char 'e'
  return $ Dictionary $ fixPair `fmap` pairs
  where
    fixPair (String s, v) = (s, v)

value :: P.Parser BValue
value = string' <|> number <|> list <|> dictionary

bstring :: Traversal' BValue ByteString
bstring f (String s) = String `fmap` f s
bstring _ bv = pure bv

bnumber :: Traversal' BValue Integer
bnumber f (Number n) = Number `fmap` f n
bnumber _ bv = pure bv

blist :: Traversal' BValue BValue
blist f (List xs) = List `fmap` traverse f xs
blist _ bv = pure bv

bkey :: ByteString -> Traversal' BValue BValue
bkey k f bv@(Dictionary m) = case lookup k m of
                               Just v -> f v
                               Nothing -> pure bv
bkey _ _ bv = pure bv

render :: BValue -> ByteString
-- renderString
render (String s)     = B.pack $ printf "%d:%s" (B.length s) (B.unpack s)
-- renderNumber
render (Number n)     = B.pack $ printf "i%de" n
-- renderList
render (List l)       = B.pack $ printf "l%se" $ B.unpack $ B.concat test
  where
    test :: [ByteString]
    test = map render l
-- renderDict
render (Dictionary d) = B.pack $ printf "d%se" $ B.unpack $ B.concat $ test
  where
    test :: [ByteString]
    test = map (\(x,y) -> B.concat [render (String x), render y]) d
