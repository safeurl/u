{-# LANGUAGE OverloadedStrings #-}

module ShortURL (getURL,
                 shortURLGen,
                 saveURL) where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BC
import qualified Database.Redis as R
import qualified System.Random as SR

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  randomDigit <- SR.randomRIO (0, length xs - 1)
  return (xs !! randomDigit)

-- increase short key length if required
shortURLGen :: IO String
shortURLGen =
  replicateM 4 (randomElement alphaNum)

getURL  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURL conn shortURI = R.runRedis conn $ R.get shortURI

-- TODO: persist in postgres as well
saveURL :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply Bool)
saveURL conn shortURL url =
  R.runRedis conn $ R.setnx shortURL url
