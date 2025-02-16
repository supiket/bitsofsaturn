{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON, ToJSON, decodeFileStrict)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (getEnv)

-- Config
-- - intervalMinutes: interval of posting
-- - r2Config: vars to fetch bits from cloudflare r2 object storage
-- - xCredentials: keys for bitsofsaturn X account's v1.1 API
data Config = Config
  { intervalMinutes :: Int,
    xCredentials :: XCredentials
  }
  deriving (Show, Generic)

-- X Config
-- - apiKey: v1.1 API key
-- - apiSecret: v1.1 API secret key
-- - accessToken: bearer token
-- - accessSecret: bearer secret
data XCredentials = XCredentials
  { apiKey :: T.Text,
    apiSecret :: T.Text,
    accessToken :: T.Text,
    accessSecret :: T.Text
  }
  deriving (Show, Generic)

-- derive JSON parsing for config types
instance FromJSON Config

instance ToJSON Config

instance FromJSON XCredentials

instance ToJSON XCredentials

-- X post content, i.e., text and media
data XPost = XPost
  { postText :: T.Text,
    mediaContent :: MediaContent
  }
  deriving (Show)

-- X media content, i.e., bytes, name and MIME type
data MediaContent = MediaContent
  { content :: BS.ByteString,
    filename :: T.Text,
    mimeType :: T.Text
  }
  deriving (Show)

-- resolve environment variables in config in the form of ${ENV_VAR_NAME}
resolveEnvVar :: T.Text -> IO T.Text
resolveEnvVar txt =
  if "${" `T.isPrefixOf` txt && "}" `T.isSuffixOf` txt
    then do
      let varName = T.unpack $ T.dropEnd 1 $ T.drop 2 txt
      envValue <- getEnv varName
      return $ T.pack envValue
    else return txt

-- load and resolve config before parsing
loadConfig :: FilePath -> IO Config
loadConfig path = do
  rawConfig <-
    decodeFileStrict path >>= \case
      Nothing -> error "Failed to parse config file"
      Just cfg -> pure cfg

  -- resolve XCredentials environment variables
  resolvedXCreds <-
    XCredentials
      <$> resolveEnvVar (apiKey $ xCredentials rawConfig)
      <*> resolveEnvVar (apiSecret $ xCredentials rawConfig)
      <*> resolveEnvVar (accessToken $ xCredentials rawConfig)
      <*> resolveEnvVar (accessSecret $ xCredentials rawConfig)

  return $
    rawConfig
      { xCredentials = resolvedXCreds
      }
