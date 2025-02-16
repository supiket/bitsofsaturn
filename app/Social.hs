{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Social
  ( postToFarcaster,
    uploadToX,
    createXPost,
  )
where

import Config (FarcasterConfig (..), XConfig (..))
import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Simple

data FarcasterPost = FarcasterPost
  { text :: T.Text,
    embeds :: [T.Text]
  }
  deriving (Show, Generic)

instance ToJSON FarcasterPost

newtype XMediaUpload = XMediaUpload
  { mediaId :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON XMediaUpload

postToFarcaster :: FarcasterConfig -> T.Text -> T.Text -> IO Bool
postToFarcaster cfg postText embedUrl = do
  let post =
        FarcasterPost
          { text = postText,
            embeds = [embedUrl]
          }
      request =
        setRequestMethod "POST" $
          setRequestHeader "Authorization" [TE.encodeUtf8 $ authToken cfg] $
            setRequestBodyJSON post "https://api.warpcast.com/v2/casts"

  response <- try $ httpNoBody request :: IO (Either SomeException (Response ()))
  case response of
    Right _ -> return True
    Left err -> do
      putStrLn $ "Error posting to Farcaster: " ++ show err
      return False

uploadToX :: XConfig -> FilePath -> IO (Maybe T.Text)
uploadToX cfg path = do
  imageData <- LBS.readFile path
  let request =
        setRequestMethod "POST" $
          setRequestHeader "Authorization" [TE.encodeUtf8 $ "Bearer " <> accessToken cfg] $
            setRequestHeader "Content-Type" ["multipart/form-data"] $
              setRequestBodyLBS imageData "https://upload.twitter.com/1.1/media/upload.json"

  response <- try (httpLBS request) :: IO (Either HttpException (Response LBS.ByteString))
  case response of
    Right res ->
      case eitherDecode (getResponseBody res) of
        Right upload -> return $ Just $ mediaId upload
        Left err -> do
          putStrLn $ "Error parsing X response: " ++ err
          return Nothing
    Left err -> do
      putStrLn $ "Error uploading to X: " ++ show err
      return Nothing

createXPost :: XConfig -> T.Text -> [T.Text] -> IO Bool
createXPost cfg postText mediaIds = do
  let params =
        [ ("status", postText),
          ("media_ids", T.intercalate "," mediaIds)
        ]
      request =
        setRequestMethod "POST" $
          setRequestHeader "Authorization" [TE.encodeUtf8 $ "Bearer " <> accessToken cfg] $
            setRequestBodyURLEncoded [(TE.encodeUtf8 k, TE.encodeUtf8 v) | (k, v) <- params] "https://api.x.com/1.1/statuses/update.json"

  response <- try $ httpNoBody request :: IO (Either SomeException (Response ()))
  case response of
    Right _ -> return True
    Left err -> do
      putStrLn $ "Error posting to X: " ++ show err
      return False
