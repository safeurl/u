{-# LANGUAGE TemplateHaskell   #-}

module STemplates (captchaTemplate,
                   createdTemplate,
                   notFoundTemplate,
                   showTemplate) where

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH

captchaTemplate :: Template
captchaTemplate = $(TH.compileMustacheFile "templates/captcha.html")

createdTemplate :: Template
createdTemplate = $(TH.compileMustacheFile "templates/created.html")

notFoundTemplate :: Template
notFoundTemplate = $(TH.compileMustacheFile "templates/notfound.html")

showTemplate :: Template
showTemplate = $(TH.compileMustacheFile "templates/show.html")
