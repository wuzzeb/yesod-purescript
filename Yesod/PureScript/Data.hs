{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Yesod.PureScript.Data
where

import Data.Text (Text)
import Yesod.Core


data PureScriptSite = PureScriptSite


mkYesodSubData "PureScriptSite" [parseRoutes|
/*Texts PureScriptCompiledR GET
|]
