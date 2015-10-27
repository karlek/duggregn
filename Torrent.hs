{-# LANGUAGE OverloadedStrings #-}

module Torrent where

import           Crypto.Hash.SHA1
import           Data.Binary.Get
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import           Data.ByteString.Lazy.Char8 (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List                  (intercalate)
import           Data.Maybe
import           Lens.Family2
import           Network.URL                (URL, importURL)

import           Bencode

readTorrent :: B.ByteString -> Torrent
readTorrent buf = case decode buf of
                  Left x        -> error x
                  Right bencode -> parseTorrent bencode

data Torrent = Torrent {
  -- Info hash of the torrent file.
  infoHash      :: Maybe B.ByteString
  -- The URL of the tracker.
, announce      :: Maybe B.ByteString
  -- Comment about the torrent's contents.
, comment       :: Maybe B.ByteString
  -- Created by author.
, createdBy     :: Maybe B.ByteString
  -- Unix epoch date.
,  creationDate :: Maybe Integer
  -- Pieces / file information.
, info          :: InfoDictionary
  -- Optional list of trackers.
-- , urlList       :: Maybe [URL]
, urlList       :: Maybe [B.ByteString]
} deriving (Show)

-- This maps to a dictionary whose keys are dependent on whether one or more
-- files are being shared.
data InfoDictionary = InfoDictionary {

  -- Size of the file in bytes (only when one file is being shared)
  len         :: Maybe Integer

  -- Suggested filename where the file is to be saved (if one file)/suggested
  -- directory name where the files are to be saved (if multiple files).
, name        :: Maybe B.ByteString

  -- Number of bytes in each piece.
, pieceLength :: Maybe Integer

  -- A hash list, i.e., a concatenation of each piece's SHA-1 hash. As SHA-1
  -- returns a 160-bit hash, pieces will be a string whose length is a multiple
  -- of 160-bits. If the torrent contains multiple files, the pieces are formed
  -- by concatenating the files in the order they appear in the files dictionary
  -- (i.e. all pieces in the torrent are the full piece length except for the
  -- last piece, which may be shorter).
, pieces      :: Maybe B.ByteString

} deriving (Show)

parseTorrent :: BValue -> Torrent
parseTorrent bencode = Torrent {
  infoHash     = bencode ^? bkey "info" >>= (Just) . hash . render
, announce     = bencode ^? bkey "announce" . bstring
, comment      = bencode ^? bkey "comment" . bstring
, createdBy    = bencode ^? bkey "created by" . bstring
, creationDate = bencode ^? bkey "creation date" . bnumber
, info         = InfoDictionary {
    len         = bencode ^? bkey "info" . bkey "length" . bnumber
  , name        = bencode ^? bkey "info" . bkey "name" . bstring
  , pieceLength = bencode ^? bkey "info" . bkey "piece length" . bnumber
  , pieces      = bencode ^? bkey "info" . bkey "pieces" . bstring
  }
, urlList      = bencode ^? bkey "url-list" >>= toUrls
}

toUrls :: BValue -> Maybe [B.ByteString]
toUrls x = Just strings
  where
      strings :: [B.ByteString]
      strings = (x ^.. blist . bstring)
      urls :: [Maybe URL]
      urls = fmap (importURL . B.unpack) strings
      clean :: [URL]
      clean = map fromJust . filter isJust $ urls
