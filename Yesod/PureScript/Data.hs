{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Yesod.PureScript.Data
where


import Control.Concurrent (MVar)
import Data.Text (Text)
import Data.Time (UTCTime)
import Yesod.Core

import qualified Data.Map.Strict as M
import qualified Filesystem.Path as FSP
import qualified Language.PureScript as P


data Mode = Dynamic | Static


-- | Per-module compile options.
data CompileOptions = CompileOptions { coMain :: Maybe Text }


-- | Options for Yesod PureScript.
data YesodPureScriptOptions = YesodPureScriptOptions {
        -- | Mode of compilation: dynamic or static.
        ypsoMode :: Mode,
        -- | Source directories to look for ".purs" files.
        ypsoSourceDirectories :: [Text],
        -- | Ignores, list of regexps that prevent parsing.
        ypsoSourceIgnores :: [Text],
        -- | Defines what and how to compile.
        ypsoCompileOptions :: [CompileOptions],
        -- | Optionally specifies div id in parent page that will be used to display compilation error if any.
        -- This is because usually resulting JS is loaded by HTML page, and it might
        -- save a time (esp for trivial errors) if error is displayed directly in HTML.
        ypsoErrorDivId :: Maybe Text,
        -- | Option passed to PureScript compiler, makes errors verbose.
        ypsoVerboseErrors :: Bool
    }


-- | This is meant to be stored in synchronized variable.
data PureScriptSiteState = PureScriptSiteState {
        -- | A flag telling us if compiler subthread is running.
        psssWatchStarted :: Bool,

        -- | Results of file parsing, time and either errors as text or list of modules,
        -- keyed by file path.
        psssModules :: M.Map FSP.FilePath (UTCTime, (Either Text [P.Module])),

        -- | Cached modules compiled with "module" and "main" options to Text.
        -- Result of compilation is either compile error(s) as Text or
        -- the Text of compiled module.
        -- Values in map are tuples of time of compilation attempt and compilation result.
        psssCompiledModules :: M.Map Text (UTCTime, (Either Text Text))
    }


-- | Yesod sub site for PureScript.
data PureScriptSite = PureScriptSite { pssState :: MVar PureScriptSiteState
                                     , pssOptions :: YesodPureScriptOptions }


mkYesodSubData "PureScriptSite" [parseRoutes|
/*Texts PureScriptCompiledR GET
|]
