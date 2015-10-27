{-# LANGUAGE OverloadedStrings #-}

module Main where

-- TODO(_): Implement multiple files in InfoDictionary.

-- http://jonas.nitro.dk/bittorrent/bittorrent-rfc.html

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad                    (forever)
import           Control.Monad                    (when)
import           Data.Binary                      (Word8)
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.List                        (genericLength, intercalate)
import           Data.List.Split                  (chunksOf)
import           Data.Maybe                       (fromJust, isNothing)
import           IPPrint.Colored                  (cpprint)
import           Network.HTTP                     (getRequest, getResponseBody,
                                                   simpleHTTP)
import           Network.HTTP.Types.URI           (renderQuery,
                                                   simpleQueryToQuery)
import           Network.IP.Addr                  (IP4, ip4ToOctetList)
import           Network.Socket
import           System.Console.CmdArgs.Verbosity
import           System.Environment               (getArgs)
import           System.Exit
import           System.IO
import           System.Timeout

import           Data.Bencode
import           Data.Torrent
-- import           Network.BitTorrent.PWP
import           Network.BitTorrent.Handshake
import           Network.BitTorrent.Tracker       (Peer (..), Tracker (..),
                                                   parseTracker)
-- | Default duration to wait for a timeout.
defaultDuration = 2*1000000

-- main :: IO ()
-- main = getArgs >>= parse >> blob

-- parse [x]            = return ()
-- parse ["-v", x]     = setVerbosity Loud

main :: IO ()
main = do
  setVerbosity Normal
  -- Get torrent file from cli-arguments.
  args <- getArgs
  when (null args) $ do
    whenNormal $ putStrLn "please gief input"
    exitFailure

  -- Extraact filename.
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
  -- cpprint $ peers track
  when (isNothing $ peers track) $ do
    whenNormal $ putStrLn "Empty peer list :("
    exitFailure
  let Just ps       = peers track

  chan <- newChan
  print (chunksOf 5 ps)
  a <- mapConcurrently (forkIO . findWilling chan meta) (chunksOf 5 ps)
  print a
  forever $ do
    handles <- readChan chan
    print handles
  whenLoud $ putStrLn "I'm alive?"

findWilling :: Chan Handle -> Torrent -> [Peer] -> IO ()
findWilling chan meta = mapM_ (\x -> connect' x `catch` handler)
    where
      handler e = do
        let err = show (e :: IOException)
        whenLoud $ hPutStrLn stderr ("[!] Warning: " ++ err)
        return ()
      connect' = reachOutToPeer chan (Data.Torrent.infoHash meta) . createAddr
      createAddr peer = SockAddrInet port' ip'
        where
          (Just ip', Just port') = (ip peer   >>= Just . decimalip,
                                    port peer >>= Just . fromIntegral)

reachOutToPeer :: Chan Handle -> Maybe B.ByteString -> SockAddr -> IO ()
reachOutToPeer chan hash addr = do
  whenLoud $ putStrLn "[.] Searching..."

  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1

  -- Check for timeout.
  r <- timeout defaultDuration $ connect sock addr
  case r of
    Nothing -> whenLoud $ putStrLn ("[!] Timed out after: " ++ show ( fromIntegral defaultDuration/1000000) ++ "s.")
    Just _  -> do
      h <- socketToHandle sock ReadWriteMode
      whenLoud $ putStrLn $ "\t[o] Connected: " ++ show addr

      -- Send handshake.
      whenLoud $ putStrLn "\t[.] Sending handshake."
      writeHandshake h (fromJust hash) myId

      -- Wait for response.
      recieved <- B.hGetSome h handshakeLen
      let parsed = parseHandshake . BL.fromStrict $ recieved
      case parsed of
        Left  (_, _, err)   -> whenLoud $ putStrLn ("\t[!] " ++ err)
        Right (_, _, their) -> do
          whenLoud $ putStrLn "\t[o] Response: "
          whenLoud $ cpprint their
          -- Compare handshakes.
          if Network.BitTorrent.Handshake.infoHash their == hash
            then do
              whenLoud $ putStrLn "\t[o] Valid handshake!"
              whenLoud $ putStrLn $ "\t[o] Handle: " ++ show h
              writeChan chan  h
            else
              whenLoud $ putStrLn ("\t[!] Invalid handshake: " ++ show recieved)

stringip :: IP4 -> String
stringip = intercalate "." . map show . ip4ToOctetList

decimalip :: (Num a) => IP4 -> a
decimalip address = sum . zipWith (curry shift) [0..] $ ip4ToOctetList address
  where
    shift :: (Num a) => (Int, Word8) -> a
    shift (x,y) = 2^(8*x)*fromIntegral y

queryTracker :: String -> IO String
queryTracker url = do
  resp <- simpleHTTP (getRequest url)
  getResponseBody resp

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
