{-|
Module      : Data.Tracker
Description : Parsing of BitTorrent trackers responses.
License     : PublicDomain
Maintainer  : Henry Eklind <henrye@kth.se>
Stability   : experimental

This package handles parsing and data extraction of BitTorrent trackers
repsonses.
-}

{-# LANGUAGE OverloadedStrings #-}

module Network.BitTorrent.Tracker (
   Tracker(..)
,  Peer(..)
,  parseTracker
) where

import           Data.Binary.Get            (getWord16be, getWord8, runGet)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (isJust)
import           Lens.Family2               ((^?))
import           Network.IP.Addr            (IP4, ip4FromOctets)

import           Data.Bencode

-- | Tracker is the parsed information from a tracker's response.
data Tracker = Tracker {
-- | Self-explanatory; fatal error.
  failure     :: Maybe ByteString
-- | Non-fatal error? Mystical return value...
, warning     :: Maybe ByteString
-- | Interval in seconds that the client should wait between sending regular
-- requests to the tracker.
, interval    :: Maybe Integer
-- | Minimum announce interval. If present, clients must not reannounce more
-- frequently than this.
, minInterval :: Maybe Integer
-- | No idea.
, trackerId   :: Maybe ByteString
-- | Number of times the file have been downloaded.
, downloaded  :: Maybe Integer
-- | Number of peers with the entire file, aka "seeders".
, complete    :: Maybe Integer
-- | Number of non-seeder peers, aka "leechers".
, incomplete  :: Maybe Integer
-- | Peers appears as two kinds in the wild.
, peers       :: Maybe [Peer]
} deriving (Show)

data Peer = Peer {
  -- | Peer's self-selected id.
  peerId :: Maybe ByteString
  -- | Peer's IP address is either: IPv6, IPv4 or DNS name.
, ip     :: Maybe IP4
  -- | Peer's port number.
, port   :: Maybe Integer
} deriving (Show)

-- | parseTracker parses a tracker response.
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
    peerType :: Maybe [Peer]
    peerType
      | isJust bin = bin >>= Just . parseBinModel
      | otherwise  = undefined

-- | parseBinModel parses one of the ways the peers field can be represented.
-- Here, every peer is 6 bytes (4 for ip and 2 for port).
parseBinModel :: ByteString -> [Peer]
parseBinModel s | B.empty == s = []
parseBinModel s = Peer {
  peerId = Nothing
, ip     = Just $ parseIP   . BL.fromStrict $ B.take 4 peer
, port   = Just $ parsePort . BL.fromStrict $ B.drop 4 peer
} : parseBinModel (B.drop 6 s)
  where
    peer = B.take 6 s

-- | parseDictModel parses one of the ways the peers field can be represented.
-- Here, every peer is a bencoded dictionary.
-- This is currently unused because no testing has been done.
-- parseDictModel :: BValue -> Peer
-- parseDictModel bencode = Peer {
--   peerId = bencode ^? bkey "peer id" . bstring
-- , ip     = bencode ^? bkey "ip" . bstring >>= Just . parseIP . BL.fromStrict
-- , port   = bencode ^? bkey "port" . bnumber
-- }

-- | parseIP converts 4 bytes into a "192.168.1.1" kind-of string.
parseIP :: BL.ByteString -> IP4
parseIP = runGet f
  where
    f = do
      a <- getWord8
      b <- getWord8
      c <- getWord8
      d <- getWord8
      return $ ip4FromOctets a b c d

-- | sparsePort converts 2 bytes into a big-endian number.
parsePort :: BL.ByteString -> Integer
parsePort = runGet f
  where
    f = getWord16be >>= return . fromIntegral
