{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Yesod.PureScript.Data
where


import Control.Concurrent (MVar)
import Data.Text (Text)
import Yesod.Core

import qualified Control.Concurrent as C
import qualified Data.Map.Strict as M
import qualified Language.PureScript as P


-- | Options for Yesod PureScript.
data YesodPureScriptOptions = YesodPureScriptOptions { ypsSourceDirectories :: [Text]
                                                     , ypsErrorDivId :: Maybe Text }


-- | This is meant to be stored in synchronized variable.
data PureScriptSiteState = PureScriptSiteState { psStateWatchStarted :: Bool
                                               , psStateModules :: M.Map Text (Either Text [P.Module]) }


-- | Yesod sub site for pure script.
data PureScriptSite = PureScriptSite { pssState :: MVar PureScriptSiteState
                                     , pssOptions :: YesodPureScriptOptions }


mkYesodSubData "PureScriptSite" [parseRoutes|
/*Texts PureScriptCompiledR GET
|]
