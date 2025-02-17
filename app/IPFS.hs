module IPFS
  ( DirectoryEntry (..),
    listPinataDirectory,
    makeIPFSUrl,
  )
where

import Config (IPFSConfig (..))
import Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject, (.:))
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest_, setRequestHeader, setRequestMethod)

data DirectoryEntry = DirectoryEntry
  { name :: T.Text,
    hash :: T.Text,
    tsize :: Integer
  }
  deriving (Show, Generic)

newtype IPFSHash = IPFSHash
  { hashValue :: T.Text
  }
  deriving (Show, Generic)

newtype IPFSDirectory = IPFSDirectory
  { links :: [DirectoryEntry]
  }
  deriving (Show, Generic)

instance FromJSON IPFSDirectory where
  parseJSON = withObject "IPFSDirectory" $ \v ->
    IPFSDirectory
      <$> v .: "Links"

instance FromJSON IPFSHash where
  parseJSON = withObject "IPFSHash" $ \v ->
    IPFSHash
      <$> v .: "/"

instance FromJSON DirectoryEntry where
  parseJSON = withObject "DirectoryEntry" $ \v -> do
    _name <- v .: "Name"
    hashObj <- v .: "Hash"
    _hashValue <- hashObj .: "/"
    _tsize <- v .: "Tsize"
    return $ DirectoryEntry _name _hashValue _tsize

-- for accessing content - uses the configured gateway
makeIPFSUrl :: IPFSConfig -> DirectoryEntry -> T.Text
makeIPFSUrl cfg entry =
  gateway cfg
    <> "/ipfs/"
    <> folderCID cfg
    <> "/"
    <> name entry

-- for listing directory contents - uses Pinata gateway
listPinataDirectory :: IPFSConfig -> IO (Either String [DirectoryEntry])
listPinataDirectory cfg = do
  let url = "https://gateway.pinata.cloud/ipfs/" <> T.unpack (folderCID cfg) <> "?format=dag-json"
      request =
        setRequestMethod "GET" $
          setRequestHeader "Authorization" [BC.pack $ "Bearer " <> T.unpack (pinataToken cfg)] $
            setRequestHeader "Accept" ["application/json"] $
              parseRequest_ url

  response <- httpLBS request
  let result = eitherDecode $ getResponseBody response
  pure $ case result of
    Left err -> Left err
    Right obj -> Right (links obj)
