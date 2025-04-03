{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( BlueskyConfig (..),
    Config (..),
    FarcasterConfig (..),
    IPFSConfig (..),
    loadConfig,
  )
where

import Data.Aeson (FromJSON, decodeFileStrict)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Config = Config
  { ipfsConfig :: IPFSConfig,
    farcasterConfig :: FarcasterConfig,
    blueskyConfig :: BlueskyConfig,
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

data BlueskyConfig = BlueskyConfig
  { blueskyIdentifier :: T.Text,
    blueskyPassword :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON Config

instance FromJSON IPFSConfig

instance FromJSON FarcasterConfig

instance FromJSON BlueskyConfig

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

resolveInterval :: T.Text -> IO Int
resolveInterval t = do
  case readMaybe (T.unpack t) of
    Just interval -> return interval
    Nothing -> do
      hPutStrLn stderr $ "error: parsing interval minutes " ++ T.unpack t
      exitFailure

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

  resolvedBlueskyConf <-
    BlueskyConfig
      <$> resolveEnvVar (blueskyIdentifier $ blueskyConfig rawConfig)
      <*> resolveEnvVar (blueskyPassword $ blueskyConfig rawConfig)

  interval <- resolveEnvVar (intervalMinutes rawConfig)
  _resolvedInterval <- resolveInterval interval

  return $
    rawConfig
      { ipfsConfig = resolvedIpfsConf,
        farcasterConfig = resolvedFarcasterConf,
        blueskyConfig = resolvedBlueskyConf,
        intervalMinutes = interval
      }
