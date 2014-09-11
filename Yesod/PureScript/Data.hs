{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Yesod.PureScript.Data
where


import Control.Concurrent (MVar)
import Data.Text (Text)
import Yesod.Core

import qualified Data.Map.Strict as M
import qualified Language.PureScript as P


-- | Options for Yesod PureScript.
data YesodPureScriptOptions = YesodPureScriptOptions { ypsSourceDirectories :: [Text]
                                                     , ypsErrorDivId :: Maybe Text }


-- | This is meant to be stored in synchronized variable.
data PureScriptSiteState = PureScriptSiteState {
        -- | A flag telling us if compiler subthread is running.
        psStateWatchStarted :: Bool,
        -- | Results of file parsing, either errors as text or list of modules,
        -- keyed by file path.
        psStateModules :: M.Map Text (Either Text [P.Module]),
        -- | Cached modules compiled with "module" and "main" options to Text.
        -- Result of compilation is either compile error(s) as Text or
        -- the Text of compiled module.
        psStateCompiledModules :: M.Map Text (Either Text Text)
    }


-- | Yesod sub site for pure script.
data PureScriptSite = PureScriptSite { pssState :: MVar PureScriptSiteState
                                     , pssOptions :: YesodPureScriptOptions }


mkYesodSubData "PureScriptSite" [parseRoutes|
/*Texts PureScriptCompiledR GET
|]
