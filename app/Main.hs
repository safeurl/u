{-# LANGUAGE OverloadedStrings #-}

module Main where

import Captcha (callCaptcha)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.HTTP.Types.Status (badRequest400, serviceUnavailable503)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai (requestHeaderHost)
import ShortURL (getURL, shortURLGen, saveURL)
import STemplates (captchaTemplate, createdTemplate, notFoundTemplate, showTemplate)
import System.Environment (getEnv, lookupEnv)
import Text.Mustache
import Web.Scotty

message503 :: TL.Text
message503 = "Service Unavailable. Please try again after some time."

paramHandleMissing :: Parsable a => TL.Text -> ActionM a
paramHandleMissing p =
    param p `rescue` (\_ -> do
                         status badRequest400
                         html $ mappend p " is missing"
                         finish)

-- TODO: log redis error
longURLResponse :: R.Connection -> BC.ByteString -> BC.ByteString -> ActionM ()
longURLResponse rConn sk hostname = do
    url <- liftIO (getURL rConn sk)
    case url of
        Left _ -> status serviceUnavailable503 >> html message503
        Right mbURL -> case mbURL of
            Nothing -> html $ renderMustache notFoundTemplate $ object ["shortKey" .= BC.unpack sk,
                                                                        "hostname" .= BC.unpack hostname]
            Just lu -> html $ renderMustache showTemplate $ object ["longURL" .= BC.unpack lu]

-- TODO: log redis error
shortURLResponse :: R.Connection -> TL.Text -> BC.ByteString -> ActionM ()
shortURLResponse rConn lu hostname = do
    shortURL <- liftIO shortURLGen
    let shorty = BC.pack shortURL
        uri' = encodeUtf8 (TL.toStrict lu)
    resp <- liftIO (saveURL rConn shorty uri')
    case resp of
      Right True -> html $ renderMustache createdTemplate $ object ["longURL"  .= lu,
                                                                    "shortURL" .= shortURL,
                                                                    "hostname" .= BC.unpack hostname]
      _          -> status serviceUnavailable503 >> html message503

-- TODO: use reader monad
-- TODO: improve error handling
-- TODO: improve Text handling
app :: R.Connection -> String -> ScottyM ()
app rConn captchaSecret = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
      addHeader "Content-Type" "text/html; charset=utf-8"
      file "static/index.html"

    post "/show" $ do
      h <- paramHandleMissing "h-captcha-response"
      sk <- paramHandleMissing "shortKey"
      hostname <- fromMaybe "" . requestHeaderHost <$> request
      captchaResult <- liftIO $ callCaptcha (TL.unpack h) captchaSecret
      case captchaResult of
          Just True -> longURLResponse rConn sk hostname
          _         -> status badRequest400 >> html "captcha check failed"

    get "/:shortURL" $ do
      su <- param "shortURL"
      html $ renderMustache captchaTemplate $ object ["shortKey" .= TL.unpack su]

-- TODO: validate long url, format, length
    post "/create" $ do
      h <- paramHandleMissing "h-captcha-response"
      lu <- paramHandleMissing "longURL"
      hostname <- fromMaybe "" . requestHeaderHost <$> request
      captchaResult <- liftIO $ callCaptcha (TL.unpack h) captchaSecret
      case captchaResult of
          Just True -> shortURLResponse rConn lu hostname
          _         -> status badRequest400 >> html "captcha check failed"


main :: IO()
main = do
    appPort <- read <$> getEnv "PORT"
    captchaSecret <- fromMaybe (error "HCAPTCHA_SECRET not found") <$> lookupEnv "HCAPTCHA_SECRET"
    connectStr <- fromMaybe (error "REDIS_URL not found") <$> lookupEnv "REDIS_URL"
    rConn <- case R.parseConnectInfo connectStr of
                  Left e -> error ("redis connection error: " ++ e)
                  Right connInfo -> R.checkedConnect connInfo
    scotty appPort (app rConn captchaSecret)
