{-# LANGUAGE OverloadedStrings #-}

module BlueSky (postWithImage) where

import Config (BlueskyConfig (..))
import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (..),
    decode,
    encode,
    object,
    (.:),
    (.=),
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client (RequestBody (..), responseBody)
import Network.HTTP.Simple
  ( Response,
    httpLBS,
    parseRequest,
    setRequestBody,
    setRequestBodyJSON,
    setRequestHeader,
  )

data SessionResponse = SessionResponse
  { accessJwt :: T.Text,
    did :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON SessionResponse

data Blob = Blob
  { cid :: T.Text,
    mimeType :: T.Text,
    size :: Int,
    ref :: Aeson.Object
  }
  deriving (Show, Generic)

instance FromJSON Blob where
  parseJSON = Aeson.withObject "Blob" $ \v ->
    Blob <$> v .: "cid" <*> v .: "mimeType" <*> v .: "size" <*> v .: "$link"

createBlueskySession :: BlueskyConfig -> IO (Either String SessionResponse)
createBlueskySession cfg = do
  let url = "https://bsky.social/xrpc/com.atproto.server.createSession"
      body =
        object
          [ "identifier" .= blueskyIdentifier cfg,
            "password" .= blueskyPassword cfg
          ]
  initReq <- parseRequest url
  let req =
        setRequestBodyJSON body $
          setRequestHeader "Content-Type" ["application/json"] initReq
  result <- try (httpLBS req) :: IO (Either SomeException (Response BL.ByteString))
  case result of
    Right response -> case decode (responseBody response) of
      Just session -> return $ Right session
      Nothing -> return $ Left "failed to parse session response"
    Left err -> return $ Left $ "error creating session: " ++ show err

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
                setRequestHeader "Content-Type" ["image/jpeg"] $ -- TODO: guess from file extension
                  initReq
      result <- try (httpLBS req) :: IO (Either SomeException (Response BL.ByteString))
      case result of
        Right response -> case decode (responseBody response) of
          Just (Object o) -> case Aeson.lookup "blob" o of
            Just blob -> return $ Right blob
            Nothing -> return $ Left "blob key missing in response"
          _ -> return $ Left "failed to parse blob upload response"
        Left err -> return $ Left $ "upload failed: " ++ show err

postWithImage :: BlueskyConfig -> T.Text -> FilePath -> T.Text -> IO Bool
postWithImage cfg text imgPath altText = do
  sessionRes <- createBlueskySession cfg
  case sessionRes of
    Left err -> putStrLn ("authentication failed: " ++ err) >> return False
    Right session -> do
      blobRes <- uploadBlob (accessJwt session) imgPath
      case blobRes of
        Left err -> putStrLn ("image upload failed: " ++ err) >> return False
        Right blobVal -> do
          now <- pure "2024-04-03T12:00:00Z" -- You can format current time here if needed
          let post =
                object
                  [ "$type" .= ("app.bsky.feed.post" :: T.Text),
                    "text" .= text,
                    "createdAt" .= now,
                    "embed"
                      .= object
                        [ "$type" .= ("app.bsky.embed.images" :: T.Text),
                          "images"
                            .= [ object
                                   [ "alt" .= altText,
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
            Right res -> do
              putStrLn "post created!"
              print (responseBody res)
              return True
            Left err -> putStrLn ("post failed: " ++ show err) >> return False
