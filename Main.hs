{-# LANGUAGE OverloadedStrings #-}

module Main where

-- TODO(_): Implement multiple files in InfoDictionary.

-- http://jonas.nitro.dk/bittorrent/bittorrent-rfc.html

import           Control.Monad          (when)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as B
import           Data.Maybe
import           IPPrint.Colored        (cpprint)
import           Network.HTTP
import           Network.HTTP.Types.URI (renderQuery, simpleQueryToQuery)
import           System.Environment     (getArgs)
import           System.Exit
import           Lens.Family2

import           Bencode
import           Torrent

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    putStrLn "please gief input"
    exitFailure
  let (filename:_) = args
  buf  <- B.readFile filename
  let meta = readTorrent buf
  when (isNothing $ announce meta) $ do
    putStrLn "No announce url"
    exitFailure
  let url = B.unpack $ B.concat [fromJust $ announce meta, query meta]
  cpprint $ infoHash meta >>= \x -> Just $ B16.encode x
  cpprint $ announce meta
  -- cpprint $ info meta
  cpprint $ query meta
  resp <- queryTracker url
  print $ decode (B.pack resp)
  case decode (B.pack resp) of
        Left x  -> print x
        Right x -> cpprint $ parseTracker x

queryTracker :: String -> IO String
queryTracker url = do
  resp <- simpleHTTP (getRequest url)
  getResponseBody resp

  -- case resp of
  --     Left x  -> print x
  --     Right x -> getResponseBody x

--   postRequestWithBody "http://tracker.archlinux.org:6969/announce"

myId :: B.ByteString
myId = "iwillneverforgetthis"

query :: Torrent -> B.ByteString
query meta =
  renderQuery True (simpleQueryToQuery
    [ ("peer_id",    myId)
    , ("info_hash",  fromJust $ infoHash meta)
    , ("comapct",    "1")
    , ("port",       "4711")
    , ("uploaded",   "0")
    , ("downloaded", "0")
    , ("left",       "0")])
    -- , ("left",       B.pack . show . fromJust . len . info $ meta)])
