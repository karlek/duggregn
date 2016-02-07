{-# LANGUAGE OverloadedStrings #-}

module Main where

-- TODO(_): Implement multiple files in InfoDictionary.

-- http://jonas.nitro.dk/bittorrent/bittorrent-rfc.html

import           Control.Exception
import           Control.Monad                    (forever, when)
import qualified Data.Binary                      as Binary
import           Data.Binary.Get
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.List                        (intercalate)
import           Data.Maybe                       (fromJust, isNothing)
import           IPPrint.Colored                  (cpprint)
import           Network.HTTP                     (getRequest, getResponseBody,
                                                   simpleHTTP)
import           Network.HTTP.Types.URI           (renderQuery,
                                                   simpleQueryToQuery)
import           Network.IP
import           Network.Socket
import           System.Console.CmdArgs.Verbosity
import           System.Environment               (getArgs)
import           System.Exit

import           System.IO
import           System.Timeout
import           Text.Printf

import           Data.Bencode                     (decode)
import           Data.Torrent
import           Network.BitTorrent.Handshake
import           Network.BitTorrent.PWP
import           Network.BitTorrent.Tracker       (Peer (..), Tracker (..),
                                                   parseTracker)
main :: IO ()
main = do
  setVerbosity Loud
  -- Get torrent file from cli-arguments.
  args <- getArgs
  when (null args) $ do
    whenNormal $ putStrLn "please gief input"
    exitFailure

  -- Extract filename.
  let (filename:_) = args
  -- Read file.
  buf  <- B.readFile filename

  -- Extract meta information.
  let meta = readTorrent buf
  when (isNothing $ announce meta) $ do
    whenNormal $ putStrLn "No announce url"
    exitFailure

  -- Debug print: info hash, anounce url and our query to tracker.
  -- cpprint $ infoHash meta >>= Just . B16.encode
  -- cpprint $ announce meta
  -- cpprint $ createTrackerQuery meta

  -- Create url to query tracker with.
  let url = createTrackerQuery meta
  -- Poke tracker for info and peers :)
  resp <- queryTracker url

  -- Decode the servers response.
  let Right r = decode (B.pack resp)
  -- Aaaand parse it.
  let track   = parseTracker r

  -- Debug print the peers!
  cpprint track
  when (isNothing $ peers track) $ do
    whenNormal $ putStrLn "Empty peer list :("
    exitFailure
  let Just ps       = peers track
  cpprint meta
  findWilling meta ps
  -- chan <- newChan
  -- print (chunksOf 5 ps)
  -- a <- mapConcurrently (forkIO . findWilling meta) (chunksOf 5 ps)
  -- print a
  -- forever $ do
  --   handle <- readChan chan
  --   print handle

  whenLoud $ putStrLn "I'm alive?"

findWilling :: Torrent -> [Peer] -> IO ()
findWilling meta = mapM_ (\x -> connect' x `catch` handler)
    where
      handler e = do
        let err = show (e :: IOException)
        whenLoud $ hPutStrLn stderr ("[!] Warning: " ++ err)
        return ()
      connect' = reachOutToPeer (Data.Torrent.infoHash meta) . createAddr
      createAddr peer = SockAddrInet port' ip'
        where
          (Just ip', Just port') = (ip peer   >>= Just . decimalip,
                                    port peer >>= Just . fromIntegral)

reachOutToPeer :: Maybe B.ByteString -> SockAddr -> IO ()
reachOutToPeer hash addr = do
  whenLoud $ putStrLn "[.] Searching..."

  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1

  -- Check for timeout.
  r <- timeout defaultDuration $ connect sock addr
  case r of
    Nothing -> timedOut
    Just _  -> handshakeSocket hash sock addr
  where
    timedOut = whenLoud $
      printf "[!] Timed out after: %fs.\n" (fromIntegral defaultDuration / 1000000 :: Double)

-- | Default duration to wait for a timeout.
-- defaultDuration :: (Fractional a) => a
defaultDuration = 2*10^6

handshakeSocket :: Maybe B.ByteString -> Socket -> SockAddr -> IO ()
handshakeSocket hash sock addr = do
  h <- socketToHandle sock ReadWriteMode
  whenLoud $ putStrLn $ "\t[o] Connected: " ++ show addr

  -- Send handshake.
  whenLoud $ putStrLn "\t[.] Sending handshake."
  writeHandshake h (fromJust hash) myId

  -- Wait for response.
  recieved <- B.hGetSome h handshakeLen
  let parsed = parseHandshake . BL.fromStrict $ recieved
  case parsed of
    Left  (_, _, err)   -> whenLoud $ printf "\t[!] %s\n" err
    Right (_, _, their) -> do
      whenLoud $ putStrLn "\t[o] Response: "
      whenLoud $ cpprint their
      -- Compare handshakes.
      if Network.BitTorrent.Handshake.infoHash their == hash
        then do
          whenLoud $ putStrLn "\t[o] Valid handshake!"
          whenLoud $ printf "\t[o] Handle: %s\n" (show h)
          -- Get length
          forever $ do
            readMsg h
        else
          whenLoud $ printf "\t[!] Invalid handshake: %s\n" (show recieved)

readMsg :: Handle -> IO ()
readMsg h = do
  putStrLn "here"
  header <- B.hGetSome h 4
  putStrLn "after"
  print header
  let pkglen = fromIntegral . runGet getWord32be $ BL.fromStrict header
  pkg <- B.hGetSome h pkglen
  putStrLn "now here"
  print pkg
  print $ (Binary.decode (BL.fromStrict $ B.append header pkg) :: PWP)

queryTracker :: String -> IO String
queryTracker url = do
  simpleHTTP (getRequest url) >>= getResponseBody

myId :: B.ByteString
myId = "iwillneverforgetthis"

createTrackerQuery :: Torrent -> String
createTrackerQuery meta = B.unpack $ B.concat [url, query]
  where
    url = fromJust . announce $ meta
    query = renderQuery True (simpleQueryToQuery
      [ ("peer_id",    myId)
      , ("info_hash",  fromJust $ Data.Torrent.infoHash meta)
      , ("comapct",    "1")
      , ("port",       "4711")
      , ("uploaded",   "0")
      , ("downloaded", "0")
      , ("left",       "0")])
    -- , ("left",       B.pack . show . fromJust . len . info $ meta)])
