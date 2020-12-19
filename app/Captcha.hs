{-# LANGUAGE OverloadedStrings #-}

module Captcha (callCaptcha) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)


hCaptchaVerifyUrl :: String
hCaptchaVerifyUrl = "https://hcaptcha.com/siteverify"

-- TODO: move to reader monad env
sitekey :: String
sitekey = "ce091196-2a6a-4eaa-864b-caf7d83e3416"


data HcaptchaResponse = HcaptchaResponse
    {success    :: Bool,
     errorcodes :: Maybe [String]}
  deriving Show

instance FromJSON HcaptchaResponse where
    parseJSON (Object v) = HcaptchaResponse
        <$> v .:  "success"
        <*> v .:? "error-codes"
    parseJSON _ = empty


-- TODO: handle captcha server down
callCaptcha :: String -> String -> IO (Maybe Bool)
callCaptcha hCaptchaResponse secret = do
    manager <- newManager tlsManagerSettings

    initialRequest <- parseRequest hCaptchaVerifyUrl
    let hpairs =
            [ ("response", BC.pack hCaptchaResponse)
            , ("secret", BC.pack secret)
            , ("sitekey", BC.pack sitekey)
            ]
        hrequest = (urlEncodedBody hpairs initialRequest)
            { method = "POST"
            }

    response <- httpLbs hrequest manager
    putStrLn $ "hcaptcha status code: "
            ++ show (statusCode $ responseStatus response)
    putStrLn $ "hcaptcha response body: "
            ++ show (responseBody response)
    return $ success <$> (decode (responseBody response) :: Maybe HcaptchaResponse)
