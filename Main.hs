module Main where

-- TODO(_): Implement multiple files in InfoDictionary.

-- http://jonas.nitro.dk/bittorrent/bittorrent-rfc.html

import           IPPrint.Colored (cpprint)
import           Control.Monad (when,void)
import qualified Data.ByteString.Char8 as B
import           System.Environment    (getArgs)

import Torrent (readTorrent)

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ void (putStrLn "please gief input")
  let (filename:_) = args
  buf  <- B.readFile filename
  let meta = readTorrent buf
  cpprint meta
