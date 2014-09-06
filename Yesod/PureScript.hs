{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Yesod.PureScript (
    PureScriptSite(PureScriptSite),
    YesodPureScript,
    YesodPureScriptOptions(YesodPureScriptOptions),
    defaultYesodPureScriptOptions,
    getPureScriptRoute
 )
where

-- PureScript Yesod Subsite
-- goal is to serve eg ./purs/foo.purs file at /purs/foo.js url.

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Either (lefts, rights)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Prelude
import System.FilePath ((</>))
import Text.Parsec (ParseError)
import Yesod.Core ( HandlerT
                  , Route
                  , TypedContent (TypedContent)
                  , Yesod
                  , YesodSubDispatch
                  , mkYesodSubDispatch
                  , toContent
                  , yesodSubDispatch )
import qualified Data.Default as DD
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as DT
import qualified Language.PureScript as P
import qualified System.Directory as D
import qualified System.IO.UTF8 as U

import Yesod.PureScript.Data


data YesodPureScriptOptions = YesodPureScriptOptions { ypsSourceDirectories :: [Text]
                                                     , ypsErrorDivId :: Maybe Text }


class Yesod master => YesodPureScript master where
    name :: HandlerT master IO Text
    name = return "purescript"


instance YesodPureScript master => YesodSubDispatch PureScriptSite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPureScriptSite)


instance DD.Default YesodPureScriptOptions where
    def = defaultYesodPureScriptOptions


type PureScriptHandler a = (YesodPureScript master) => HandlerT PureScriptSite (HandlerT master IO) a


defaultYesodPureScriptOptions :: YesodPureScriptOptions
defaultYesodPureScriptOptions = YesodPureScriptOptions { ypsSourceDirectories = ["purs", "bower_components"]
                                                       , ypsErrorDivId = Nothing }


getPureScriptRoute :: [Text] -> Route PureScriptSite
getPureScriptRoute p = PureScriptCompiledR p


getPureScriptCompiledR :: [Text] -> PureScriptHandler TypedContent
getPureScriptCompiledR p = do
    let jsModulePath = T.intercalate "." p  -- foo.bar.baz[.js]
    let jsModuleName0 = if T.isSuffixOf ".js" jsModulePath
            then T.dropEnd 3 jsModulePath
            else jsModulePath
    let jsModuleName = DT.trace ("js module name: " ++ show jsModuleName0) jsModuleName0
    compileResult <- liftIO $ compilePureScriptFile ["purs", "bower_components"] jsModuleName
    case compileResult of
        Left err -> do
            let errbs = T.encodeUtf8 err
            return (TypedContent "text/plain" (toContent errbs))
        Right js -> do
            let jsbs = T.encodeUtf8 js
            return (TypedContent "application/javascript" (toContent jsbs))


-- Take dir, find '*.purs' files recursively.
findFiles :: Text -> IO [Text]
findFiles dir = do
    let dirp = T.unpack dir
    allNames <- D.getDirectoryContents dirp
    let goodNames = filter (flip notElem [".", ".."]) allNames
    pathLists <- forM goodNames $ \n -> do
        let p = dirp </> n
        isDir <- D.doesDirectoryExist p
        if isDir
            then findFiles (T.pack p)
            else return $ if isSuffixOf ".purs" p then [T.pack p] else []
    let paths = (concat pathLists)
    return paths


parseFile :: Text -> IO (Either ParseError [P.Module])
parseFile fn = do
    fileContents <- U.readFile (T.unpack (DT.trace ("parsing " ++ show fn) fn))
    return $ P.runIndentParser (T.unpack fn) P.parseModules fileContents


-- Compile modules, return either parse error or parsed modules.
readInput :: [Text] -> IO (Either ParseError [P.Module])
readInput sourceDirs = do
    let lsActions = map (\d -> findFiles d) sourceDirs
    dirsFiles <- sequence lsActions
    let fileNames = concat dirsFiles :: [Text]
    parseResults <- mapM parseFile fileNames
    let parseErrors = lefts parseResults :: [ParseError]
    let parsedModules = concat (rights parseResults) :: [P.Module]
    case parseErrors of
        [] -> return (Right parsedModules)
        (x:_) -> return (Left x)


compilePureScriptFile :: [Text] -> Text -> IO (Either Text Text)
compilePureScriptFile sourceDirs moduleName = do
    let psOptions = P.defaultOptions { P.optionsModules = [T.unpack moduleName]
                                     , P.optionsMain = Just (T.unpack moduleName)
                                     , P.optionsPerformRuntimeTypeChecks = False  -- XXX exception in generated code when enabled
                                     , P.optionsNoPrelude = False
                                     , P.optionsBrowserNamespace = Just "PS" }    -- XXX fromJust error when Nothing
    parsed <- readInput sourceDirs
    case parsed of
        Left _ -> return $ Left "parse error" -- XXX
        Right modules -> do
            let compileResult = DT.trace "compiled modules" $ P.compile psOptions (DT.trace "about to compile modules" modules)
            case compileResult of
                Left err -> return $ Left (T.pack err)
                Right (js, _, _) -> do
                    return $ Right (T.pack js)


