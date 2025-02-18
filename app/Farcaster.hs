{-# LANGUAGE OverloadedStrings #-}

module Farcaster (postCastWithImage) where

import Config (FarcasterConfig (..))
import Control.Exception (SomeException, try)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (Response, getResponseStatusCode, httpLBS, parseRequest, setRequestBodyLBS, setRequestHeader)

postCastWithImage :: FarcasterConfig -> T.Text -> T.Text -> IO Bool
postCastWithImage cfg castText imgUrl = do
  let apiKey = neynarApiKey cfg
  let signerUuid = neynarUuid cfg

  let embedObject = object ["url" .= imgUrl] :: Value
  let requestBody =
        encode $
          object
            [ "text" .= castText,
              "signer_uuid" .= signerUuid,
              "embeds" .= [embedObject]
            ]

  initReq <- parseRequest "POST https://api.neynar.com/v2/farcaster/cast"
  let request =
        setRequestBodyLBS requestBody $
          setRequestHeader "api_key" [TE.encodeUtf8 apiKey] $
            setRequestHeader "Content-Type" ["application/json"] initReq

  result <- try (httpLBS request) :: IO (Either SomeException (Response BL.ByteString))

  case result of
    Right response -> do
      putStrLn $ "response body: " ++ show (responseBody response)
      let status = getResponseStatusCode response
      if status >= 200 && status < 300
        then return True
        else do
          putStrLn $ "failed to post cast. status code: " ++ show status
          return False
    Left err -> do
      putStrLn $ "error sending request: " ++ show err
      return False
