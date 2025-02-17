{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    FarcasterConfig (..),
    IPFSConfig (..),
    loadConfig,
    deserializeInterval,
  )
where

import Data.Aeson (FromJSON, decodeFileStrict)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Config = Config
  { ipfsConfig :: IPFSConfig,
    farcasterConfig :: FarcasterConfig,
    intervalMinutes :: T.Text
  }
  deriving (Show, Generic)

data IPFSConfig = IPFSConfig
  { gateway :: T.Text,
    folderCID :: T.Text,
    pinataToken :: T.Text
  }
  deriving (Show, Generic)

data FarcasterConfig = FarcasterConfig
  { neynarApiKey :: T.Text,
    neynarUuid :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON Config

instance FromJSON IPFSConfig

instance FromJSON FarcasterConfig

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
      <*> resolveEnvVar (pinataToken $ ipfsConfig rawConfig)

  resolvedFarcasterConf <-
    FarcasterConfig
      <$> resolveEnvVar (neynarApiKey $ farcasterConfig rawConfig)
      <*> resolveEnvVar (neynarUuid $ farcasterConfig rawConfig)

  resolvedInterval <- resolveEnvVar (intervalMinutes rawConfig)

  return $
    rawConfig
      { ipfsConfig = resolvedIpfsConf,
        farcasterConfig = resolvedFarcasterConf,
        intervalMinutes = resolvedInterval
      }
