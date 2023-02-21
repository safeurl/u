{-# LANGUAGE OverloadedStrings #-}

module Captcha (callCaptcha) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)


captchaVerifyUrl :: String
captchaVerifyUrl = "https://challenges.cloudflare.com/turnstile/v0/siteverify"

-- TODO: move to reader monad env
sitekey :: String
sitekey = "0x4AAAAAAACpydFGHpGE9tPa"


data CaptchaResponse = CaptchaResponse
    {success    :: Bool,
     errorcodes :: Maybe [String]}
  deriving Show

instance FromJSON CaptchaResponse where
    parseJSON (Object v) = CaptchaResponse
        <$> v .:  "success"
        <*> v .:? "error-codes"
    parseJSON _ = empty


-- TODO: handle captcha server down
callCaptcha :: String -> String -> IO (Maybe Bool)
callCaptcha captchaResponse secret = do
    manager <- newManager tlsManagerSettings

    initialRequest <- parseRequest captchaVerifyUrl
    let tpairs =
            [ ("response", BC.pack captchaResponse)
            , ("secret", BC.pack secret)
            , ("sitekey", BC.pack sitekey)
            ]
        trequest = (urlEncodedBody tpairs initialRequest)
            { method = "POST"
            }

    response <- httpLbs trequest manager
    putStrLn $ "captcha status code: "
            ++ show (statusCode $ responseStatus response)
    putStrLn $ "captcha response body: "
            ++ show (responseBody response)
    return $ success <$> (decode (responseBody response) :: Maybe CaptchaResponse)
