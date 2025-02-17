{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    XConfig (..),
    FarcasterConfig (..),
    IPFSConfig (..),
    loadConfig,
    deserializeInterval,
  )
where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Config = Config
  { ipfsConfig :: IPFSConfig,
    farcasterConfig :: FarcasterConfig,
    xConfig :: XConfig,
    intervalMinutes :: T.Text
  }
  deriving (Show, Generic)

data IPFSConfig = IPFSConfig
  { gateway :: T.Text,
    folderCID :: T.Text
  }
  deriving (Show, Generic)

newtype FarcasterConfig = FarcasterConfig
  { authToken :: T.Text
  }
  deriving (Show, Generic)

data XConfig = XConfig
  { apiKey :: T.Text,
    apiSecret :: T.Text,
    accessToken :: T.Text,
    accessSecret :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON Config

instance FromJSON IPFSConfig

instance FromJSON FarcasterConfig

instance FromJSON XConfig

resolveEnvVar :: T.Text -> IO T.Text
resolveEnvVar t = case T.stripPrefix "${" t >>= T.stripSuffix "}" of
  Just var -> do
    mValue <- lookupEnv (T.unpack var)
    case mValue of
      Just value -> pure $ T.pack value
      Nothing -> do
        hPutStrLn stderr $ "warning: environment variable " ++ T.unpack var ++ " not found, using empty string"
        pure T.empty
  Nothing -> pure t

deserializeInterval :: T.Text -> Int
deserializeInterval t =
  let defaultValue = 240
   in case readMaybe (T.unpack t) of
        Just n -> n
        Nothing -> defaultValue

loadConfig :: FilePath -> IO Config
loadConfig path = do
  rawConfig <-
    decodeFileStrict path >>= \case
      Nothing -> error "failed to parse config file"
      Just cfg -> pure cfg

  resolvedIpfsConf <-
    IPFSConfig
      <$> resolveEnvVar (gateway $ ipfsConfig rawConfig)
      <*> resolveEnvVar (folderCID $ ipfsConfig rawConfig)

  resolvedFarcasterConf <-
    FarcasterConfig
      <$> resolveEnvVar (authToken $ farcasterConfig rawConfig)

  resolvedXConf <-
    XConfig
      <$> resolveEnvVar (apiKey $ xConfig rawConfig)
      <*> resolveEnvVar (apiSecret $ xConfig rawConfig)
      <*> resolveEnvVar (accessToken $ xConfig rawConfig)
      <*> resolveEnvVar (accessSecret $ xConfig rawConfig)

  resolvedInterval <- resolveEnvVar (intervalMinutes rawConfig)

  return $
    rawConfig
      { ipfsConfig = resolvedIpfsConf,
        farcasterConfig = resolvedFarcasterConf,
        xConfig = resolvedXConf,
        intervalMinutes = resolvedInterval
      }
