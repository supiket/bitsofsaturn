module Main where

import Config
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Farcaster
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import IPFS
import Network.HTTP.Simple (HttpException, Response, getResponseBody, httpLBS, parseRequest)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.Random (randomRIO)

downloadImage :: T.Text -> FilePath -> IO Bool
downloadImage url path = do
  request <- parseRequest (T.unpack url)
  response <- try $ httpLBS request :: IO (Either HttpException (Response BL.ByteString)) -- Add type annotation
  case response of
    Right r -> do
      BL.writeFile path (getResponseBody r)
      return True
    Left err -> do
      putStrLn $ "error downloading image: " ++ show err
      return False

isValidImage :: T.Text -> Bool
isValidImage fileName =
  let lowercase = T.toLower fileName
   in T.isSuffixOf ".jpg" lowercase
        && not (T.isPrefixOf "." lowercase) -- excludes .DS_Store and other hidden files
        && not (T.isPrefixOf "_" lowercase) -- excludes _thumb files etc
        && T.all isAllowedChar fileName -- ensures filename only contains safe characters
  where
    isAllowedChar c = isAlphaNum c || c `elem` ['.', '-', '_']

processFile :: Config -> DirectoryEntry -> IO ()
processFile cfg entry = do
  let ipfsUrl = makeIPFSUrl (ipfsConfig cfg) entry
  putStrLn $ "processing: " ++ T.unpack (name entry)
  let tempPath = "temp" </> T.unpack (name entry)
  createDirectoryIfMissing True "temp"
  downloadSuccess <- downloadImage ipfsUrl tempPath
  if downloadSuccess
    then do
      let postText = "A bit of Saturn"

      farcasterSuccess <- postCastWithImage (farcasterConfig cfg) postText ipfsUrl
      putStrLn $ "farcaster post: " ++ if farcasterSuccess then "success" else "failed"

      removeFile tempPath
    else putStrLn "failed to download image"

sleep :: Int -> IO ()
sleep minutes = do
  let microseconds = minutes * 60 * 1000000
  putStrLn $ "\nsleeping for " ++ show minutes ++ " minutes..."
  threadDelay microseconds

main :: IO ()
main = forever $ do
  setLocaleEncoding utf8
  cfg <- loadConfig "config.json"
  entries <- listPinataDirectory (ipfsConfig cfg)
  case entries of
    Left err -> do
      putStrLn $ "error listing directory: " ++ err
      sleep (deserializeInterval (intervalMinutes cfg))
    Right entries' -> do
      let validFiles = filter (isValidImage . name) entries'
      putStrLn $ "found " ++ show (length validFiles) ++ " valid image files"
      case validFiles of
        [] -> do
          putStrLn "no files to process"
          sleep (deserializeInterval (intervalMinutes cfg))
        bits -> do
          randomIndex <- randomRIO (0, length bits - 1)
          let selectedBit = bits !! randomIndex
          putStrLn $ "randomly selected: " ++ T.unpack (name selectedBit)
          processFile cfg selectedBit
          sleep (deserializeInterval (intervalMinutes cfg))
