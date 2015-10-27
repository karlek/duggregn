{-|
Module      : Data.Bencode
Description : Encoding and decoding of bencoded strings.
License     : PublicDomain
Maintainer  : Henry Eklind <henrye@kth.se>
Stability   : experimental

This package handles encoding, decoding and data extraction of bencoded strings.
-}

-- TODO(_): I have no idea what this does.
-- Used by bkey. Don't know what this does yet.
{-# LANGUAGE RankNTypes #-}

module Data.Bencode where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as B
import           Lens.Family2
import           Text.Printf

-- | BValue is all possible values a bencoded string can represent.
data BValue = String ByteString
            | Number Integer
            | List [BValue]
            | Dictionary [(ByteString, BValue)] deriving (Show)

-- | Decode a buffer of bencoded data to recieve a BValue parse tree.
decode :: ByteString -> Either String BValue
decode = P.parseOnly value

-- | String' parses a bencoded string, e.g: `4:love`.
-- | Represents: "love"
string' :: P.Parser BValue
string' = do
  n <- P.decimal
  void $ P.char ':'
  String `fmap` P.take n

-- | Number parses a bencoded number, e.g: `i4711e`
-- | Represents: 4711
number :: P.Parser BValue
number = do
  void $ P.char 'i'
  n <- P.decimal
  void $ P.char 'e'
  return $ Number n

-- | List parses a bencoded list, e.g: `l5:peacee`
-- | Represents: ["peace"]
list :: P.Parser BValue
list = do
  void $ P.char 'l'
  xs <- P.many' value
  void $ P.char 'e'
  return $ List xs

-- | Dictionary parses a bencoded dictionary, e.g: `d3:key4:doore`
-- | Represents: {"key": "door"}
dictionary :: P.Parser BValue
dictionary = do
  void $ P.char 'd'
  pairs <- P.many' ((,) `fmap` string' <*> value)
  void $ P.char 'e'
  return $ Dictionary $ fixPair `fmap` pairs
  where
    fixPair (String s, v) = (s, v)
    fixPair _             = undefined

-- | TODO(_): Refactor this comment.
-- | value is used for parsing type-agnostic (? need help with this comment :P)
-- | states.
value :: P.Parser BValue
value = string' <|> number <|> list <|> dictionary

-- | TODO(_): I have no idea what this does.
bstring :: Traversal' BValue ByteString
bstring f (String s) = String `fmap` f s
bstring _ bv = pure bv

-- | TODO(_): I have no idea what this does.
bnumber :: Traversal' BValue Integer
bnumber f (Number n) = Number `fmap` f n
bnumber _ bv = pure bv

-- | TODO(_): I have no idea what this does.
blist :: Traversal' BValue BValue
blist f (List xs) = List `fmap` traverse f xs
blist _ bv = pure bv

-- | TODO(_): I have no idea what this does.
bkey :: ByteString -> Traversal' BValue BValue
bkey k f bv@(Dictionary m) = case lookup k m of
                               Just v -> f v
                               Nothing -> pure bv
bkey _ _ bv = pure bv

-- | Render a BValue into a bencoded string.
render :: BValue -> ByteString
-- renderString
render (String s) = B.pack $ printf "%d:%s" (B.length s) (B.unpack s)
-- renderNumber
render (Number n) = B.pack $ printf "i%de" n
-- renderList
render (List l) = B.pack . printf "l%se" . B.unpack . B.concat $ map render l
-- renderDict
render (Dictionary d) = B.pack . printf "d%se" . B.unpack . B.concat $ vals
  where
    vals :: [ByteString]
    vals = map (\(x,y) -> B.concat [render (String x), render y]) d
