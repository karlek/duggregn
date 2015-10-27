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


data Tracker = Tracker {
-- Self-explanatory; fatal error.
  failure     :: Maybe B.ByteString
-- Non-fatal error? Mystical return value...
, warning     :: Maybe B.ByteString
-- Interval in seconds that the client should wait between sending regular
-- requests to the tracker.
, interval    :: Maybe Integer
-- Minimum announce interval. If present, clients must not reannounce more
-- frequently than this.
, minInterval :: Maybe Integer
, trackerId   :: Maybe B.ByteString
-- Number of times the file have been downloaded.
, downloaded  :: Maybe Integer
-- Number of peers with the entire file, aka "seeders".
, complete    :: Maybe Integer
-- Number of non-seeder peers, aka "leechers".
, incomplete  :: Maybe Integer
-- Peers appears as two kinds in the wild.
, peers       :: Maybe [Peer]
} deriving (Show)

data Peer = Peer {
  -- Peer's self-selected id.
  peerId :: Maybe B.ByteString
  -- Peer's IP address is either: IPv6, IPv4 or DNS name.
, ip     :: Maybe B.ByteString
  -- Peer's port number.
, port :: Maybe Integer
} deriving (Show)


parseTracker :: BValue -> Tracker
parseTracker bencode = Tracker {
  failure     = bencode ^? bkey "failure" . bstring
, warning     = bencode ^? bkey "warning" . bstring
, interval    = bencode ^? bkey "interval" . bnumber
, minInterval = bencode ^? bkey "min interval" . bnumber
, trackerId   = bencode ^? bkey "tracker id" . bstring
, downloaded  = bencode ^? bkey "downloaded" . bnumber
, complete    = bencode ^? bkey "complete" . bnumber
, incomplete  = bencode ^? bkey "incomplete" . bnumber
, peers       = peerType
}
  where
    bin = bencode ^? bkey "peers" . bstring
    -- dic = map parseDictModel (bencode ^? bkey "peers" ^.. blist)
    peerType :: Maybe [Peer]
    peerType
      | isJust bin = bin >>= Just . parseBinModel
      | otherwise  = undefined

parseBinModel :: B.ByteString -> [Peer]
parseBinModel s | B.empty == s = []
parseBinModel s = Peer {
  peerId = Nothing
, ip     = Just $ parseIP   . fromStrict $ B.take 4 peer
, port   = Just $ parsePort . fromStrict $ B.drop 4 peer
} : parseBinModel (B.drop 6 s)
  where
    peer = B.take 6 s

parseDictModel :: BValue -> Peer
parseDictModel bencode = Peer {
  peerId = bencode ^? bkey "peer id" . bstring
, ip     = bencode ^? bkey "ip" . bstring
, port   = bencode ^? bkey "port" . bnumber
}

parseIP :: BL.ByteString -> B.ByteString
parseIP = runGet f
  where
    f = do
      a <- getWord8
      b <- getWord8
      c <- getWord8
      d <- getWord8
      return $ B.pack . intercalate "." $ map show [a, b, c, d]

parsePort :: BL.ByteString -> Integer
parsePort = runGet f
  where
    f = do
      port <- getWord16be
      return $ fromIntegral port
