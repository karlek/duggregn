{-# LANGUAGE OverloadedStrings #-}

module Tracker where

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8   as BL
import Lens.Family2
import Data.Binary.Get
import Data.Maybe
import Data.List (intercalate)

import Bencode

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
, ip     = Just $ parseIP   . BL.fromStrict $ B.take 4 peer
, port   = Just $ parsePort . BL.fromStrict $ B.drop 4 peer
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
    f = getWord16be >>= return . fromIntegral
