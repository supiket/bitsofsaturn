{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)
import Types (intervalMinutes, loadConfig)

main :: IO ()
main = do
  config <- loadConfig "config.json"
  let _delayMicros = intervalMinutes config * 60 * 1000000
  hFlush stdout
