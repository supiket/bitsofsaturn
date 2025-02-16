module IPFS
  ( IPFSFile (..),
    listIPFSDirectory,
    makeIPFSUrl,
    isJPG,
  )
where

import Config (IPFSConfig (..))
import Control.Exception (try)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Simple

-- file representation matching IPFS directory listing
data IPFSFile = IPFSFile
  { name :: T.Text, -- file name with path
    hash :: T.Text, -- file's CID
    size :: Integer,
    type_ :: T.Text -- "File" or "Directory"
  }
  deriving (Show, Generic)

instance FromJSON IPFSFile where
  parseJSON = withObject "IPFSFile" $ \v ->
    IPFSFile
      <$> v .: "name"
      <*> v .: "hash"
      <*> v .: "size"
      <*> v .: "type"

-- directory listing from IPFS gateway
newtype DirectoryListing = DirectoryListing
  { entries :: [IPFSFile]
  }
  deriving (Show, Generic)

instance FromJSON DirectoryListing

makeIPFSUrl :: IPFSConfig -> T.Text -> T.Text
makeIPFSUrl cfg path =
  gateway cfg
    <> "/ipfs/"
    <> folderCID cfg
    <> (if T.null path then "" else "/" <> path)

isJPG :: T.Text -> Bool
isJPG filename =
  any
    (`T.isSuffixOf` T.toLower filename)
    [".jpg", ".jpeg"]

listIPFSDirectory :: IPFSConfig -> IO [IPFSFile]
listIPFSDirectory cfg = do
  let url = gateway cfg <> "/ipfs/" <> folderCID cfg <> "?format=json"
  request <- parseRequest (T.unpack url)
  response <- try (httpLBS request) :: IO (Either HttpException (Response LBS.ByteString))
  case response of
    Right res -> do
      case eitherDecode (getResponseBody res) of
        Right listing -> return $ filter (\f -> type_ f == "File") $ entries listing
        Left err -> do
          putStrLn $ "error parsing directory listing: " ++ err
          return []
    Left err -> do
      putStrLn $ "error fetching directory listing: " ++ show err
      return []
