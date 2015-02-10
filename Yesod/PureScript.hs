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
    addPureScriptWidget,
    createYesodPureScriptSite,
    defaultYesodPureScriptOptions,
    getPureScriptRoute,
    yesodPureScript
 )
where

-- PureScript Yesod Subsite
-- goal is to serve eg ./purs/foo.purs file at /purs/foo.js url.

import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException)
import Control.Monad (forever, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Either (rights)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Filesystem.Path ((</>))
import Formatting
import Formatting.Time
import Language.Haskell.TH
import Language.PureScript (Module(Module))
import Prelude
import Text.Julius
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import Yesod.Core ( HandlerT
                  , Route
                  , TypedContent (TypedContent)
                  , Yesod
                  , YesodSubDispatch
                  , addScript
                  , getYesod
                  , mkYesodSubDispatch
                  , shamlet
                  , toContent
                  , toTypedContent
                  , toWidget
                  , yesodSubDispatch )
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as CM
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.Default as DD
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Filesystem as FS
import qualified Filesystem.Path as FSP
import qualified Filesystem.Path.CurrentOS as FSPC
import qualified Language.PureScript as P
import qualified System.FSNotify as SFN


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
        , ypsoSourceIgnores = []
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
    let fnsmodules = mapMaybe _justSndRight (M.toAscList moduleMap) :: [(FSP.FilePath, (UTCTime, [Module]))]

    -- (fileName, error)
    let fnerrs = mapMaybe _justSndLeft (M.toAscList moduleMap) :: [(FSP.FilePath, (UTCTime, Text))]

    let _formatTime _t = format (dateDash % " " % hms) _t _t

    let filePathToText fp = case FSPC.toText fp of
            Left _t -> _t
            Right _t -> _t

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
                                        <td>#{filePathToText fn}
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
                                                <td>#{filePathToText fn}
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
                    let errbs = TE.encodeUtf8 err
                    return (TypedContent "text/plain" (toContent errbs))
                Just _id -> do
                    let _errtxt = TL.fromStrict err
                    let _errjs = createJavaScriptError (TL.fromStrict _id) _errtxt
                    return (TypedContent "text/javascript" (toContent _errjs))
        Right _js -> do
            let _jsbs = TE.encodeUtf8 _js
            return (TypedContent "application/javascript" (toContent _jsbs))


-- | Called by file-watching thread after loading module from purs file.
-- Updates in-memory cache of modules.
-- Result of compilation is either textual representation of parse errors or module.
-- Cache of loaded modules is key-ed by purs filename - this way we can efficiently handle
-- file deletions or renames.
addModule :: PureScriptSite -> FSPC.FilePath -> (UTCTime, Either Text [P.Module]) -> IO ()
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
removeModule :: PureScriptSite -> FSP.FilePath -> IO ()
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
                _parsed <- parseFile fp
                _now <- getCurrentTime
                addModule pureScriptSite fp (_now, _parsed)
        case (event, mext) of
            (SFN.Added _ _, Just "purs") -> _upsert
            (SFN.Modified _ _, Just "purs") -> _upsert
            (SFN.Removed _ _, Just "purs") -> do removeModule pureScriptSite fp
            _ -> return ()


-- | Start file-watching stuff if not already started.
ensureWatchStarted :: PureScriptSite -> IO ()
ensureWatchStarted pureScriptSite = do
    let mode = ypsoMode $ pssOptions pureScriptSite
    case mode of
        Dynamic -> do
            CM.modifyMVar_ (pssState pureScriptSite) $ \state -> do
                case psssWatchStarted state of
                    False -> do
                        _m <- parseSiteFiles pureScriptSite
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


matchPath :: Text -> FSP.FilePath -> Bool
matchPath pattern path = case FSPC.toText path of
        Left _t -> False
        Right _t -> _t =~ pattern


matchPathAny :: [Text] -> FSP.FilePath -> Bool
matchPathAny patterns path = any (flip matchPath path) patterns


matchPathNone :: [Text] -> FSP.FilePath -> Bool
matchPathNone ignores path = not $ matchPathAny ignores path


-- | Take directory FilePath, find '*.purs' files recursively.
findFiles :: [Text] -> FSPC.FilePath -> IO [FSPC.FilePath]
findFiles ignores dir = do
    allNames <- FS.listDirectory dir -- listdir foo will return foo/bar if bar is in foo, path not just name
    let goodNames = filter (matchPathNone ignores) $ filter (flip notElem [".", ".."]) allNames
    pathLists <- forM goodNames $ \n -> do
        isDir <- FS.isDirectory n
        if isDir
            then findFiles ignores n
            else do
                return $ if FSP.hasExtension n "purs" then [n] else []
    let paths = concat pathLists
    return paths


-- | High level parse interface for PureScript.
-- Takes file path as Text, returns either error as Text or parsed modules.
-- Note: PureScript file can define more than one module.
parseFile :: FSP.FilePath -> IO (Either Text [P.Module])
parseFile fn = do
    let fns = FSPC.encodeString fn
    fileContents <- T.unpack <$> TE.decodeUtf8 <$> BS.readFile fns
    let eem = case P.lex fns fileContents of
            Right _tokens -> P.runTokenParser fns P.parseModules _tokens
            Left _err -> Left _err
    let r = case eem of
            Left _e -> Left . T.pack . show $ _e
            Right m -> Right m
    return r


-- | Find files in directories and parse them.
parseFiles :: [FSP.FilePath] -> [Text] -> IO (M.Map FSPC.FilePath (UTCTime, Either Text [P.Module]))
parseFiles dirs ignores = do
    let lsActions = map (findFiles ignores) dirs
    filenames <- concat <$> sequence lsActions
    _time <- getCurrentTime
    parseResults <- forM filenames $ \fn -> do
        _parsed <- parseFile fn
        return (fn, (_time, _parsed))
    return $ M.fromList $ parseResults


-- | Parse modules, return map of path -> parse result: either parse error or parsed modules.
-- Current impl uses Filesystem.FilePath in hopes that it's faster than String-based impl and
-- that it handles edge cases better.
parseSiteFiles :: PureScriptSite -> IO (M.Map FSPC.FilePath (UTCTime, Either Text [P.Module]))
parseSiteFiles pureScriptSite = parseFiles dirs ignores
    where
        ypso = pssOptions pureScriptSite
        dirs = map FSPC.fromText $ ypsoSourceDirectories ypso :: [FSP.FilePath]
        ignores = ypsoSourceIgnores ypso :: [Text]


preludeModules :: [P.Module]
preludeModules = case P.lex "" P.prelude of
        Right _tokens -> case P.runTokenParser "" P.parseModules _tokens of
            Right _ms -> _ms
            Left _err -> []
        Left _err -> []


-- | Compile PureScript modules in "--main <module>" mode.
compilePureScript :: YesodPureScriptOptions -> [Module] -> Text -> Either Text Text
compilePureScript ypso modules mainModuleName = case _result of
        Left _err -> Left (T.pack _err)
        Right (_js, _, _) -> Right (T.pack _js)
    where
        _result = P.compile _psOptions modules ["yesod-purescript"]
        _psOptions = P.defaultCompileOptions { P.optionsMain = Just (T.unpack mainModuleName)
                                             , P.optionsNoPrelude = False
                                             , P.optionsAdditional = _compileOptions
                                             , P.optionsVerboseErrors = ypsoVerboseErrors ypso }
        _compileOptions = P.CompileOptions "PS" [T.unpack mainModuleName] []

-- | Takes PureScriptSite and module name and tries to compile given module in "--main <module>" mode.
-- PureScriptSite contains parsed modules.
-- Compiled module is stored (cached) in PureScriptSite's state with time of compilation.
-- Only compile result is returned.
-- Cached and returned result of compilation has type of Text.
compilePureScriptFile :: PureScriptSite -> Text -> IO (Either Text Text)
compilePureScriptFile pureScriptSite moduleName = do
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
                let _loadedModules = concat $ rights $ map snd $ M.elems _lmm
                let _modules = concat [preludeModules, _loadedModules]
                let compileResult = compilePureScript (pssOptions pureScriptSite) _modules moduleName
                let newmap = M.insert moduleName (_time, compileResult) _m
                let newstate = state { psssCompiledModules = newmap }
                return (newstate, compileResult)
    return compileResult


-- | PureScript Template Haskell
addPureScriptWidget :: YesodPureScriptOptions -> Text -> Q Exp
addPureScriptWidget ypso moduleName = do
    let dirs = map FSPC.fromText $ ypsoSourceDirectories ypso
    let ignores = ypsoSourceIgnores ypso
    parsed <- runIO $ parseFiles dirs ignores
    -- parsed is a crazy struct and for now we need only list of modules
    let parsedModules = concat $ catMaybes $ flip map (M.elems parsed) $ \(_, e) -> case e of
            Left _ -> Nothing
            Right _modules -> Just _modules
    let modules = concat [preludeModules, parsedModules]
    compiled <- case compilePureScript ypso modules moduleName of
            Left _err -> fail $ "Failed to compile PureScript module \"" ++ show moduleName ++ "\": " ++ show _err
            Right _js -> return _js
    let thLit = litE $ stringL $ T.unpack compiled -- this is string literal we can insert at TH call site
    [|toWidget $ toJavascript $ rawJS $ T.pack $(thLit)|]


-- | Either add link to dynamically compiled PureScript or return statically compiled PureScript.
-- yesodPureScript :: Bool -> Route -> YesodPureScriptOptions -> Text -> Q Exp
yesodPureScript dev routeName ypso moduleName =
        if dev then
            [|addScript $ $(conE routeName) $ getPureScriptRoute $ map T.pack [$(thModuleNameStrLit)]|]
        else
            addPureScriptWidget ypso moduleName
    where
        thModuleNameStrLit = litE $ stringL $ T.unpack moduleName


