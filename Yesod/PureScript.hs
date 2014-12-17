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
    PureScriptSite,
    YesodPureScript,
    YesodPureScriptOptions(..),
    createYesodPureScriptSite,
    defaultYesodPureScriptOptions,
    getPureScriptRoute
 )
where

-- PureScript Yesod Subsite
-- goal is to serve eg ./purs/foo.purs file at /purs/foo.js url.

import Control.Exception (catch, SomeException)
import Control.Monad (forever, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Either (rights)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Formatting
import Formatting.Time
import Language.PureScript (Module(Module))
import Prelude
import System.FilePath ((</>))
import Text.Julius
import Yesod.Core ( HandlerT
                  , Route
                  , TypedContent (TypedContent)
                  , Yesod
                  , YesodSubDispatch
                  , getYesod
                  , mkYesodSubDispatch
                  , shamlet
                  , toContent
                  , toTypedContent
                  , yesodSubDispatch )
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as CM
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Default as DD
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Filesystem as FS
import qualified Filesystem.Path as FSP
import qualified Filesystem.Path.CurrentOS as FSPC
import qualified Language.PureScript as P
import qualified System.Directory as D
import qualified System.FSNotify as SFN
import qualified System.IO.UTF8 as U

import Yesod.PureScript.Data


-- | Things that are Yesod master can also be YesodPureScript master.
class Yesod master => YesodPureScript master


-- | All things that are "YesodPureScript master", are also this other thing,
-- because they /nobody knows, because TH/.
instance YesodPureScript master => YesodSubDispatch PureScriptSite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPureScriptSite)


-- | YesodPureScriptOptions thing is also Data.Default.Default, because it has "def",
-- which is defaultYesodPureScriptOptions.
instance DD.Default YesodPureScriptOptions where
    def = defaultYesodPureScriptOptions


-- | Shortcut type for handlers of this subsite.
type PureScriptHandler a = (YesodPureScript master) => HandlerT PureScriptSite (HandlerT master IO) a


-- | Default options for YesodPureScript.
-- Needed when creating PureScriptSite.
-- Please don't create YesodPureScriptOptions by calling constructor directly,
-- so I can add more options without breaking your code.
defaultYesodPureScriptOptions :: YesodPureScriptOptions
defaultYesodPureScriptOptions = YesodPureScriptOptions
        { ypsoSourceDirectories = ["purs", "bower_components"]
        , ypsoErrorDivId = Nothing
        , ypsoVerboseErrors = False
        , ypsoMode = Dynamic
        , ypsoCompileOptions = [] }


-- | Create pure script site.
-- Initialises MVar of compiled modules to empty map.
createYesodPureScriptSite :: YesodPureScriptOptions -> IO PureScriptSite
createYesodPureScriptSite opts = do
    let state = PureScriptSiteState { psssWatchStarted = False
                                    , psssModules = M.empty
                                    , psssCompiledModules = M.empty }
    mv <- CM.newMVar state
    return $ PureScriptSite { pssState = mv
                            , pssOptions = opts }


-- | For convenience: turns a path as list into a route.
getPureScriptRoute :: [Text] -> Route PureScriptSite
getPureScriptRoute p = PureScriptCompiledR p


-- | Create JS error report that tries to insert itself into parent page.
createJavaScriptError :: TL.Text -> TL.Text -> TL.Text
createJavaScriptError errorDivId errorText = renderJavascriptUrl render tmpl
    where
        render _ _ = error "no links supported here"
        _s _tl = AesonTypes.String (TL.toStrict _tl)
        tmpl = [julius|
                var err = #{_s errorText};
                var errorDiv = document.getElementById(#{_s errorDivId});
                if (window && window.console && window.console.log) {
                    window.console.log(err);
                }
                if (errorDiv) {
                    var errnode = document.createTextNode(err);
                    var prenode = document.createElement("pre");
                    prenode.appendChild(errnode);
                    if (errorDiv.firstChild) {
                        errorDiv.insertBefore(prenode, errorDiv.firstChild);
                    } else {
                        errorDiv.appendChild(errnode);
                    }
                }
            |]


getPureScriptInfo :: PureScriptSite -> PureScriptHandler TypedContent
getPureScriptInfo site = do
    -- map of filename to either err module
    moduleMap <- liftIO $ CM.withMVar (pssState site) $ \state -> return (psssModules state)

    -- this is to filter map items whose values are tuples of (a0, Either ...),
    -- and return those items with keys as tuples of (key, (a0, unboxed right)).
    -- so this is pattern match on snd of tuple that either gives tuple of unboxed right in just or nothing...
    let _justSndRight (_k, (_t, _mv)) = case _mv of
            Right _v -> Just (_k, (_t, _v))
            _ -> Nothing

    -- similar to _justSndRight above, but returns value from Left (or nothing)
    let _justSndLeft (_k, (_t, _mv)) = case _mv of
            Left _v -> Just (_k, (_t, _v))
            _ -> Nothing

    -- (fn, [module])
    let fnsmodules = mapMaybe _justSndRight (M.toAscList moduleMap) :: [(Text, (UTCTime, [Module]))]

    -- (fileName, error)
    let fnerrs = mapMaybe _justSndLeft (M.toAscList moduleMap) :: [(Text, (UTCTime, Text))]

    let _formatTime _t = format (dateDash % " " % hms) _t _t

    return $ toTypedContent $ [shamlet|
        $doctype 5
        <html>
            <style>
                body {
                    font-family: sans-serif;
                    font-size: 10pt;
                }
            <body>
                <h1>yesod-purescript Status

                <h2>Failed Modules

                $case fnerrs
                    $of []
                        <p>All modules loaded without errors
                    $of _
                        <table>
                            <thead>
                                <tr>
                                    <th>File name
                                    <th>Error message
                                    <th>Load time
                            <tbody>
                                $forall (fn, (time, err)) <- fnerrs
                                    <tr>
                                        <td>#{fn}
                                        <td>#{err}
                                        <td>#{_formatTime time}

                <h2>Loaded Modules

                $case fnsmodules
                    $of []
                        <p>No modules loaded
                    $of _
                        <table>
                            <thead>
                                <tr>
                                    <th>Module
                                    <th>File name
                                    <th>Load time
                            <tbody>
                                $forall fnmods <- fnsmodules
                                    $with (fn, (time, modules)) <- fnmods
                                        $forall (Module name _ _) <- modules
                                            <tr>
                                                <td>#{show name}
                                                <td>#{fn}
                                                <td>#{_formatTime time}
        |]
    where



-- | This is the main handler, "entry point" for YesodPureScript.
-- It takes a path to module as a list of Text, then it parses and
-- compiles stuff and returns results to client.
getPureScriptCompiledR :: [Text] -> PureScriptHandler TypedContent
getPureScriptCompiledR [] = do
    me <- getYesod
    liftIO $ ensureWatchStarted me
    getPureScriptInfo me

getPureScriptCompiledR p = do
    me <- getYesod
    liftIO $ ensureWatchStarted me
    let jsModulePath = T.intercalate "." p  -- foo.bar.baz[.js]
    let jsModuleName = if T.isSuffixOf ".js" jsModulePath
            then T.dropEnd 3 jsModulePath
            else jsModulePath
    compileResult <- liftIO $ compilePureScriptFile me jsModuleName
    case compileResult of
        Left err -> do
            case ypsoErrorDivId (pssOptions me) of
                Nothing -> do
                    let errbs = T.encodeUtf8 err
                    return (TypedContent "text/plain" (toContent errbs))
                Just _id -> do
                    let _errtxt = TL.fromStrict err
                    let _errjs = createJavaScriptError (TL.fromStrict _id) _errtxt
                    return (TypedContent "text/javascript" (toContent _errjs))
        Right _js -> do
            let _jsbs = T.encodeUtf8 _js
            return (TypedContent "application/javascript" (toContent _jsbs))


-- | Called by file-watching thread after loading module from purs file.
-- Updates in-memory cache of modules.
-- Result of compilation is either textual representation of parse errors or module.
-- Cache of loaded modules is key-ed by purs filename - this way we can efficiently handle
-- file deletions or renames.
addModule :: PureScriptSite -> Text -> (UTCTime, Either Text [P.Module]) -> IO ()
addModule pureScriptSite fileName eitherErrOrModules = do
    CM.modifyMVar_ (pssState pureScriptSite) $ \state -> do
        let curmap = psssModules state
        let newmap = M.insert fileName eitherErrOrModules curmap
        -- For now re-loading of module causes cache all compiled modules to be
        -- dropped.
        let newstate = state { psssModules = newmap
                             , psssCompiledModules = M.empty }
        return newstate


-- | Executed when file disappears from watched dirs,
-- removes module from PureScriptSite's state.
removeModule :: PureScriptSite -> Text -> IO ()
removeModule pureScriptSite fileName = do
    CM.modifyMVar_ (pssState pureScriptSite) $ \state -> do
        let curmap = psssModules state
        let newmap = M.delete fileName curmap
        let newstate = state { psssModules = newmap
                             , psssCompiledModules = M.empty }
        return newstate



-- | Executed on file change. Updates loaded modules MVar-ed in PureScriptSite.
handleFileEvent :: PureScriptSite -> SFN.Event -> IO ()
handleFileEvent pureScriptSite event = do
        let fp = SFN.eventPath event
        let mext = FSP.extension fp
        let _upsert = do
                _parsed <- parseFile (fp2t fp)
                _now <- getCurrentTime
                addModule pureScriptSite (fp2t fp) (_now, _parsed)
        case (event, mext) of
            (SFN.Added _ _, Just "purs") -> _upsert
            (SFN.Modified _ _, Just "purs") -> _upsert
            (SFN.Removed _ _, Just "purs") -> do
                removeModule pureScriptSite (fp2t fp)
            _ -> do
                -- ignore this event
                return ()
    where
        fp2t fp = case FSPC.toText fp of
            Left _ -> error "invalid path"
            Right _t -> _t


-- | Start file-watching stuff if not already started.
ensureWatchStarted :: PureScriptSite -> IO ()
ensureWatchStarted pureScriptSite = do
    let mode = ypsoMode $ pssOptions pureScriptSite
    case mode of
        Dynamic -> do
            CM.modifyMVar_ (pssState pureScriptSite) $ \state -> do
                case psssWatchStarted state of
                    False -> do
                        _m <- parseAllFiles pureScriptSite
                        startWatchThread pureScriptSite
                        return (state { psssWatchStarted = True
                                      , psssModules = _m
                                      , psssCompiledModules = M.empty })
                    _ -> return state
        Static -> error "YPS mode is Static, can't start watch thread"


-- | Creates thread that watches input dirs and fires events on files.
-- This way modified files are instantly loaded, making dev setup more responsive.
startWatchThread :: PureScriptSite -> IO ()
startWatchThread pureScriptSite = do
        _ <- C.forkIO $ do
            let opts = pssOptions pureScriptSite
            let dirs = ypsoSourceDirectories opts
            SFN.withManager $ \mgr -> do
                forM_ dirs $ \dir -> do
                    SFN.watchTree mgr (FSPC.fromText dir) (const True) $ \e -> do
                        catch
                            (handleFileEvent pureScriptSite e)
                            (\_e -> do
                                -- XXX: catching all is bad
                                let msg = show (_e :: SomeException)
                                TIO.putStrLn $ T.concat ["exception in handleFileEvent: ", T.pack msg])
                forever $ do
                    C.threadDelay (12 * 3600 * 1000 * 1000)
        return ()


-- | Take human-readable dir name, find '*.purs' files recursively.
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


-- | High level parse interface for PureScript.
-- Takes file path as Text, returns either error as Text or parsed modules.
-- Note: PureScript file can define more than one module.
parseFile :: Text -> IO (Either Text [P.Module])
parseFile fn = do
    -- TIO.putStrLn $ T.concat ["parsing \"", fn, "\""]
    fileContents <- U.readFile (T.unpack fn)
    let eem = P.runIndentParser (T.unpack fn) P.parseModules fileContents
    let r = case eem of
            Left _e -> Left . T.pack . show $ _e
            Right m -> Right m
    -- TIO.putStrLn $ T.concat ["parsed \"", fn, "\""]
    return r


-- | Parse modules, return map of path -> parse result: either parse error or parsed modules.
-- This beautiful code tries to make sure that paths are absolute.
-- Current impl uses Filesystem.FilePath in hopes that it's faster than String-based impl and
-- that it handles edge cases better.
parseAllFiles :: PureScriptSite -> IO (M.Map Text (UTCTime, Either Text [P.Module]))
parseAllFiles pureScriptSite = do
    let sourceDirs = ypsoSourceDirectories $ pssOptions pureScriptSite
    let lsActions = map (\d -> findFiles d) sourceDirs
    dirsFiles <- sequence lsActions
    let relFileNames = map FSPC.fromText $ concat dirsFiles
    cwd <- FS.getWorkingDirectory
    let absFileNames = map (FSP.append cwd) relFileNames
    mParseResults <- forM absFileNames $ \afn -> do
        case FSPC.toText afn of
            Left _ -> return Nothing
            Right _t -> do
                _time <- getCurrentTime
                _parsed <- parseFile _t
                return $ Just (_t, (_time, _parsed))
    let parseResults = catMaybes mParseResults
    return $ M.fromList $ parseResults


-- | Takes PureScriptSite and module name and tries to compile given module in "--main <module>" mode.
-- PureScriptSite contains parsed modules.
-- Compiled module is stored (cached) in PureScriptSite's state with time of compilation.
-- Only compile result is returned.
-- Cached and returned result of compilation has type of Text.
compilePureScriptFile :: PureScriptSite -> Text -> IO (Either Text Text)
compilePureScriptFile pureScriptSite moduleName = do
    let compileOptions = P.CompileOptions "PS" [T.unpack moduleName] []
    let psOptions = P.defaultCompileOptions { P.optionsMain = Just (T.unpack moduleName)
                                            , P.optionsNoPrelude = False
                                            , P.optionsAdditional = compileOptions
                                            , P.optionsVerboseErrors = ypsoVerboseErrors (pssOptions pureScriptSite) }
    compileResult <- CM.modifyMVar (pssState pureScriptSite) $ \state -> do
        let _m = psssCompiledModules state
        case M.lookup moduleName _m of
            Just (_t, _cmt) -> do
                TIO.putStrLn $ T.concat ["compile result for js module \"", moduleName, "\" found in cache"]
                return (state, _cmt)
            Nothing -> do
                TIO.putStrLn $ T.concat ["compiling js module \"", moduleName, "\""]
                _time <- getCurrentTime
                -- No cached compile result in map, need to actually compile.
                let _lmm = psssModules state
                let _preludeModules = case P.runIndentParser "" P.parseModules P.prelude of
                        Right _ms -> _ms
                        Left _err -> []
                let _loadedModules = concat $ rights $ map snd $ M.elems _lmm
                let _modules = concat [_preludeModules, _loadedModules]
                let compileResultRaw = P.compile psOptions _modules ["yesod-purescript"]
                let compileResult = case compileResultRaw of
                        Left errStr -> Left (T.pack errStr)
                        Right (_js, _, _) -> Right (T.pack _js)
                let newmap = M.insert moduleName (_time, compileResult) _m
                let newstate = state { psssCompiledModules = newmap }
                return (newstate, compileResult)
    return compileResult

