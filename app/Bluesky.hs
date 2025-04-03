{-# LANGUAGE OverloadedStrings #-}

module Bluesky (postWithImage) where

import Config (BlueskyConfig (..))
import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (..),
    decode,
    eitherDecode,
    encode,
    object,
    (.:),
    (.=),
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Network.HTTP.Client (RequestBody (..), responseBody)
import Network.HTTP.Simple
  ( Response,
    getResponseStatusCode,
    httpLBS,
    parseRequest,
    setRequestBody,
    setRequestBodyJSON,
    setRequestBodyLBS,
    setRequestHeader,
  )
import Network.HTTP.Types.Status (statusCode)

data SessionResponse = SessionResponse
  { accessJwt :: T.Text,
    did :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON SessionResponse

data CreateRecordResponse = CreateRecordResponse
  { uri :: T.Text,
    cid :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON CreateRecordResponse

data Blob = Blob
  { blobCid :: T.Text,
    mimeType :: T.Text,
    size :: Int,
    ref :: Aeson.Object
  }
  deriving (Show, Generic)

instance FromJSON Blob where
  parseJSON = Aeson.withObject "Blob" $ \v ->
    Blob <$> v .: "cid" <*> v .: "mimeType" <*> v .: "size" <*> v .: "$link"

formatAsIso8601 :: IO T.Text
formatAsIso8601 = do
  now <- getCurrentTime
  return $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now

createBlueskySession :: BlueskyConfig -> IO (Either String SessionResponse)
createBlueskySession cfg = do
  let url = "https://bsky.social/xrpc/com.atproto.server.createSession"
      payload =
        object
          [ "identifier" .= blueskyIdentifier cfg,
            "password" .= blueskyPassword cfg
          ]
      requestBody = encode payload

  initReq <- parseRequest "POST https://bsky.social/xrpc/com.atproto.server.createSession"
  let request =
        setRequestBodyLBS requestBody $
          setRequestHeader "Content-Type" ["application/json"] $
            setRequestHeader "Accept" ["application/json"] $
              setRequestHeader "User-Agent" ["bits-of-saturn/0.1"] $
                initReq

  result <- try (httpLBS request) :: IO (Either SomeException (Response BL.ByteString))
  case result of
    Left err -> return $ Left $ "error sending request: " ++ show err
    Right response -> do
      let status = getResponseStatusCode response
          body = responseBody response

      if status >= 200 && status < 300
        then case eitherDecode body of
          Right session -> return $ Right session
          Left err -> return $ Left $ "json decode error: " ++ err
        else return $ Left "authentication failed (check handle and password)"

uploadBlob :: T.Text -> FilePath -> IO (Either String Value)
uploadBlob jwt imgPath = do
  imgBytes <- BL.readFile imgPath
  if BL.length imgBytes > 1000000
    then return $ Left "image too large (>1MB)"
    else do
      initReq <- parseRequest "POST https://bsky.social/xrpc/com.atproto.repo.uploadBlob"
      let req =
            setRequestBody (RequestBodyLBS imgBytes) $
              setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 jwt] $
                setRequestHeader "Content-Type" ["image/jpeg"] $
                  initReq
      result <- try (httpLBS req) :: IO (Either SomeException (Response BL.ByteString))
      case result of
        Right response -> case decode (responseBody response) of
          Just (Object o) -> case KM.lookup "blob" o of
            Just blob -> return $ Right blob
            Nothing -> return $ Left "blob key missing in response"
          _ -> return $ Left "failed to parse blob upload response"
        Left err -> return $ Left $ "upload failed: " ++ show err

postWithImage :: BlueskyConfig -> T.Text -> FilePath -> IO Bool
postWithImage cfg text imgPath = do
  sessionRes <- createBlueskySession cfg
  case sessionRes of
    Left err -> putStrLn ("authentication failed: " ++ err) >> return False
    Right session -> do
      blobRes <- uploadBlob (accessJwt session) imgPath
      case blobRes of
        Left err -> putStrLn ("image upload failed: " ++ err) >> return False
        Right blobVal -> do
          createdAt <- formatAsIso8601
          let post =
                object
                  [ "$type" .= ("app.bsky.feed.post" :: T.Text),
                    "text" .= text,
                    "createdAt" .= createdAt,
                    "embed"
                      .= object
                        [ "$type" .= ("app.bsky.embed.images" :: T.Text),
                          "images"
                            .= [ object
                                   [ "alt" .= text,
                                     "image" .= blobVal
                                   ]
                               ]
                        ]
                  ]
          initReq <- parseRequest "POST https://bsky.social/xrpc/com.atproto.repo.createRecord"
          let req =
                setRequestBodyJSON
                  ( object
                      [ "repo" .= did session,
                        "collection" .= ("app.bsky.feed.post" :: T.Text),
                        "record" .= post
                      ]
                  )
                  $ setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 (accessJwt session)]
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ initReq
          result <- try (httpLBS req) :: IO (Either SomeException (Response BL.ByteString))
          case result of
            Left err -> putStrLn ("post failed: " ++ show err) >> return False
            Right res -> do
              let body = responseBody res
              case eitherDecode body :: Either String CreateRecordResponse of
                Right postResponse -> do
                  putStrLn "post created!"
                  print postResponse
                  return True
                Left err -> do
                  putStrLn "post response did not match expected format:"
                  putStrLn ("decode error: " ++ err)
                  BL8.putStrLn body
                  return False
