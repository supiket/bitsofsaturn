module Main where

import Config
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import IPFS
import Network.HTTP.Simple
import Social
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

processFile :: Config -> IPFSFile -> IO ()
processFile cfg file = do
  let ipfsUrl = makeIPFSUrl (ipfsConfig cfg) (name file)
  putStrLn $ "processing: " ++ T.unpack (name file)
  let tempPath = "temp" </> T.unpack (name file)
  createDirectoryIfMissing True "temp"

  downloadSuccess <- downloadImage ipfsUrl tempPath
  if downloadSuccess
    then do
      let postText = "A bit of Saturn"

      farcasterSuccess <- postToFarcaster (farcasterConfig cfg) postText ipfsUrl
      putStrLn $ "farcaster post: " ++ if farcasterSuccess then "success" else "failed"

      mediaIdMaybe <- uploadToX (xConfig cfg) tempPath
      case mediaIdMaybe of
        Just mediaId -> do
          xSuccess <- createXPost (xConfig cfg) postText [mediaId]
          putStrLn $ "X post: " ++ if xSuccess then "success" else "failed"
        Nothing -> putStrLn "failed to upload image to X"

      removeFile tempPath
    else putStrLn "failed to download image"

sleep :: Int -> IO ()
sleep minutes = do
  let microseconds = minutes * 60 * 1000000
  putStrLn $ "\nsleeping for " ++ show minutes ++ " minutes..."
  threadDelay microseconds

main :: IO ()
main = forever $ do
  cfg <- loadConfig "config.json"

  files <- listIPFSDirectory (ipfsConfig cfg)
  let jpgFiles = filter (isJPG . name) files

  putStrLn $ "found " ++ show (length jpgFiles) ++ " JPG files"

  case jpgFiles of
    [] -> do
      putStrLn "no files to process"
      sleep (intervalMinutes cfg)
    bits -> do
      -- Select a random file
      randomIndex <- randomRIO (0, length bits - 1)
      let selectedBit = bits !! randomIndex

      putStrLn $ "randomly selected: " ++ T.unpack (name selectedBit)
      processFile cfg selectedBit
      sleep (intervalMinutes cfg)
