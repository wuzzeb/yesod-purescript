{-# LANGUAGE QuasiQuotes, TemplateHaskell, DataKinds, OverloadedStrings, TupleSections #-}
-- | Embed compiled PureScript into the 'EmbeddedStatic' subsite.
--
-- This module is designed to operate on PureScript projects organized the same as
-- <https://github.com/bodil/pulp pulp>, the PureScript build tool.  This module does not use pulp
-- directly so you do not need to have it installed, but we use the same file layout so that your
-- project can be built either using this module or pulp.
--
-- = Project Structure
--
-- I recommend a file structure such as the following:
--
-- >root
-- > - bower.json
-- > - myproject.cabal
-- > - src/server/
-- > - src/purescript/
-- > - test/server/
-- > - test/purescript/
-- > - ...
--
-- Move all the Yesod library code into the @src\/server\/@ directory, the Haskell yesod-test code
-- into the @test\/server\/@ directory.  Next, place all your PureScript modules and foreign
-- javascript into @src\/purescript\/@, and put your PureScript test code into @test\/purescript\/@.
-- You can then use commands such as @pulp psci@, @pulp docs@, and @pulp test@ to operate on your
-- PureScript project code.  (The reason for using @src\/purescript@ is that pulp searches the
-- entire @src@ subdirectory for PureScript code and it is not possible to to configure the search
-- directory in pulp.  Since you need a @src@ directory anyway, the Haskell code might as well be
-- moved into there as well.  If you do not plan on using pulp, you can configure the search
-- directory of this module by changing 'psSourceDirectory'.)
--
-- In your bower.json file, list all the PureScript dependencies you need.  Both pulp and this
-- module look for files matching @bower_components\/purescript-*\/src\/**\/*.purs@, so just listing
-- the dependency in @bower.json@ and running @bower update@ is enough.
--
-- = The EmbeddedStatic subsite
--
-- This module integrates the PureScript code using the 'EmbeddedStatic' subsite.
-- For example, if you use the project structure described above and also include bootstrap
-- in your bower.json file, you can create a module such as the following:
--
-- >module StaticFiles where
-- >
-- >import Yesod.EmbeddedStatic
-- >import Yesod.PureScript.EmbeddedGenerator
-- >
-- >#ifdef DEVELOPMENT
-- >#define DEV_BOOL True
-- >#else
-- >#define DEV_BOOL False
-- >#endif
-- >mkEmbeddedStatic DEV_BOOL "myStatic" [
-- > 
-- >    purescript "js/mypurescript.js" defaultPsGeneratorOptions
-- >        { psProductionMinimizer = uglifyJs
-- >        }
-- > 
-- >  , embedFileAt "css/bootstrap.min.css" "bower_components/bootstrap/dist/boostrap.min.css"
-- >  , embedDirAt "fonts" "bower_components/bootstrap/dist/fonts"
-- >]
-- >
-- >getStatic :: a -> EmbeddedStatic
-- >getStatic = const myStatic
--
-- You must then add the EmbeddedStatic subsite into your yesod app.  For example, in your routes
-- add:
--
-- >/static StaticR EmbeddedStatic getStatic
--
-- Alternatively, you can store @myStatic@ in your foundation datatype.
--
-- = Referencing purescript from handlers
--
-- In your handlers, reference the routes created by the generators.  That is, the @StaticFiles@
-- module will contain definitions @js_mypurescript_js@ and @css_bootstrap_min_css@, both of type
-- @Route EmbeddedStatic@, which you can reference from the widget using @addScript@ and
-- @addStylesheet@.  You can then use the @PS@ prefix inside julius code to call into the PureScript
-- code (you typically would just call a @main@ function).
--
-- >someHandler :: Handler Html
-- >someHandler = defaultLayout $ do
-- >    addStylesheet $ StaticR css_bootstrap_min_css
-- >    addScript $ StaticR js_mypurescript_js
-- >    toWidget [julius|PS.SomeModule.someFunction();|]
-- >    ...
module Yesod.PureScript.EmbeddedGenerator(
    purescript
  , PsGeneratorOptions(..)
  , defaultPsGeneratorOptions
  , PsModuleRoots(..)
) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Default (def)
import Data.IORef
import Data.Maybe (catMaybes)
import Language.Haskell.TH.Syntax (Lift(..), liftString)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.IO (hPutStrLn, stderr)
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Types

import qualified Language.PureScript as P
import qualified Language.PureScript.Bundle as B
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CodeGen.JS as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Map as M

-- | Specify PureScript modules for the roots for dead code eliminiation
data PsModuleRoots = AllSourceModules
                        -- ^ All modules located in the 'psSourceDirectory' will be used as roots.
                   | SpecifiedModules [String]
                        -- ^ The specified module names will be used as roots.

instance Lift PsModuleRoots where
    lift AllSourceModules = [| AllSourceModules |]
    lift (SpecifiedModules mods) = [| SpecifiedModules $(lift mods) |]

-- | The options to the generator.
data PsGeneratorOptions = PsGeneratorOptions {
    psSourceDirectory :: FilePath
        -- ^ The source directory containing PureScript modules.  All files recursively with a @purs@ extension
        -- will be loaded as PureScript code, and all files recursively with a @js@ extension will
        -- be loaded as foreign javascript.
  , psDependencySrcGlobs :: [String]
        -- ^ A list of globs (input to 'glob') for dependency PureScript modules.
  , psDependencyForeignGlobs :: [String]
        -- ^ A list of globs (input to 'glob') for dependency foreign javascript.
  , psDeadCodeElim :: PsModuleRoots
        -- ^ The module roots to use for dead code eliminiation.  All identifiers reachable from
        -- these modules will be kept.
  , psProductionMinimizer :: BL.ByteString -> IO BL.ByteString
        -- ^ Javascript minifier such as 'uglifyJs' to use when compiling for production.
        --   This is not used when compiling for development.
  , psDevBuildDirectory :: FilePath
        -- ^ Directory used for caching the compiler output during development mode.  The
        -- directory will be created if it does not yet exist.  This directory can be
        -- removed/cleaned at any time as long as a build is not in progress.
}

instance Lift PsGeneratorOptions where
    lift opts = [| PsGeneratorOptions $(liftString $ psSourceDirectory opts)
                                      $(lift $ psDependencySrcGlobs opts)
                                      $(lift $ psDependencyForeignGlobs opts)
                                      $(lift $ psDeadCodeElim opts)
                                      return -- the lift instance is used only for development mode, where the minimizer is not used
                                      $(liftString $ psDevBuildDirectory opts)
                |]

-- | Default options for the generator.
--
--   * The default source directory is @src\/purescript@.
--
--   * The dependencies are loaded from @bower_components\/purescript-*\/src\/@.  Thus if you list
--   all your dependencies in @bower.json@, this generator will automatically pick them all up.
--
--   * 'AllSourceModules' is used for dead code elimination, which means all modules located in the
--   @src\/purescript@ directory are used as roots for dead code elimination.  That is, all code
--   reachable from a module from the @src\/purescript@ directory is kept, and all other code from the
--   dependencies is thrown away.
--
--   * No production minimizer is configured.
--
--   * The dev build directory is @.yesod-purescript-build@
defaultPsGeneratorOptions :: PsGeneratorOptions
defaultPsGeneratorOptions = PsGeneratorOptions
  { psSourceDirectory = "src" </> "purescript"
  , psDependencySrcGlobs = ["bower_components/purescript-*/src/**/*.purs"]
  , psDependencyForeignGlobs = ["bower_components/purescript-*/src/**/*.js"]
  , psDeadCodeElim = AllSourceModules
  , psProductionMinimizer = return
  , psDevBuildDirectory = ".yesod-purescript-build"
  }

-- | Compile a PureScript project to a single javascript file.
--
-- When executing in development mode, the directory 'psDevBuildDirectory' is used to cache compiler
-- output. Every time a HTTP request for the given 'Location' occurs, the generator re-runs the
-- equivalent of @psc-make@. This recompiles any changed modules (detected by the file modification
-- time) and then bundles and serves the new javascript.  This allows you to change the PureScript
-- code or even add new PureScript modules, and a single refresh in the browser will recompile and
-- serve the new javascript without having to recompile/restart the Yesod server.
--
-- When compiling for production, the PureScript compiler will be executed to compile all PureScript
-- code and its dependencies.  The resulting javascript is then minimized, compressed, and embdedded
-- directly into the binary generated by GHC.  Thus you can distribute your compiled Yesod server
-- without having to distribute any PureScript code or JavaScript files.  (This also means any
-- changes to the PureScript code requires a re-compile of the Haskell module containing the
-- call to 'purescript').
--
-- All generated JavaScript code will be available under the global @PS@ variable. Thus from julius
-- inside a yesod handler, you can access exports from modules via something like
-- @[julius|PS.modulename.someexport("Hello, World")|]@.  There will not be any call to a main
-- function; you can call the main function yourself from julius inside your handler.
purescript :: Location -> PsGeneratorOptions -> Generator
purescript loc opts = do
    return [def
      { ebHaskellName = Just $ pathToName loc
      , ebLocation = loc
      , ebMimeType = "application/javascript"
      , ebProductionContent = compileAndBundle loc opts ModeProduction >>= psProductionMinimizer opts
      , ebDevelReload = [| compileAndBundle $(liftString loc) $(lift opts) ModeDevelopment |]
      }]

data MakeMode = ModeDevelopment | ModeProduction
    deriving (Show, Eq)

type ParseOutput = ([(Either P.RebuildPolicy FilePath, P.Module)], M.Map P.ModuleName (FilePath, P.ForeignJS))
type CompilerOutput = [(B.ModuleIdentifier, String)]

-- | Helper function to parse the purescript modules
parse :: [(FilePath, String)] -> [(FilePath, String)] -> WriterT P.MultipleErrors (Either P.MultipleErrors) ParseOutput
parse files foreign =
    (,) <$> P.parseModulesFromFiles (either (const "") id) (map (\(fp,str) -> (Right fp, str)) files)
        <*> P.parseForeignModulesFromFiles foreign

-- | Compile and bundle the purescript
compileAndBundle :: Location -> PsGeneratorOptions -> MakeMode -> IO BL.ByteString
compileAndBundle loc opts mode = do
    hPutStrLn stderr $ "Compiling " ++ loc

    srcNames <- glob (psSourceDirectory opts </> "**/*.purs")
    depNames <- concat <$> mapM glob (psDependencySrcGlobs opts)
    foreignNames <- concat <$> mapM glob
        ((psSourceDirectory opts </> "**/*.js") : psDependencyForeignGlobs opts)
    psFiles <- mapM (\f -> (f,) <$> readFile f) $ srcNames ++ depNames
    foreignFiles <- mapM (\f -> (f,) <$> readFile f) foreignNames

    case runWriterT (parse psFiles foreignFiles) of
        Left err -> do
            hPutStrLn stderr $ P.prettyPrintMultipleErrors False err
            case mode of
                ModeProduction -> error "Error parsing purescript"
                ModeDevelopment -> return $ TL.encodeUtf8 $ TL.pack $ P.prettyPrintMultipleErrors False err
        Right (parseOutput, warnings) -> do
            when (P.nonEmpty warnings) $
                hPutStrLn stderr $ P.prettyPrintMultipleWarnings False warnings

            case mode of
                ModeDevelopment -> do
                    compileOutput <- compileDevel opts parseOutput
                    bundleOutput <- either (return . Left) (bundle opts srcNames parseOutput) compileOutput
                    case bundleOutput of
                        Left err -> return err
                        Right js -> return js

                ModeProduction -> do
                    compileOutput <- compileProd parseOutput
                    bundleOutput <- bundle opts srcNames parseOutput compileOutput
                    case bundleOutput of
                        Left _err -> error "Error while bundling javascript"
                        Right js -> return js

-- | Compile for development mode, using the disk-based make mode.
compileDevel :: PsGeneratorOptions -> ParseOutput -> IO (Either BL.ByteString CompilerOutput)
compileDevel opts (ms, foreigns) = do
    let filePathMap = M.fromList $ map (\(fp, P.Module _ mn _ _) -> (mn, fp)) ms

        actions = P.buildMakeActions (psDevBuildDirectory opts) filePathMap foreigns False
        compileOpts = P.defaultOptions { P.optionsNoOptimizations = True
                                       , P.optionsVerboseErrors = True
                                       }
    e <- P.runMake compileOpts $ P.make actions ms

    case e of
        Left err -> do
            hPutStrLn stderr $ P.prettyPrintMultipleErrors False err
            return $ Left $ TL.encodeUtf8 $ TL.pack $ P.prettyPrintMultipleErrors False err
        Right (_, warnings') -> do
            when (P.nonEmpty warnings') $
                hPutStrLn stderr $ P.prettyPrintMultipleWarnings False warnings'

            indexJs <- forM ms $ \(_, P.Module _ mn _ _) -> do
                idx <- readFile $ psDevBuildDirectory opts </> P.runModuleName mn </> "index.js"
                return (B.ModuleIdentifier (P.runModuleName mn) B.Regular, idx)

            return $ Right indexJs


-- | In-memory cache of purescript compiler output
data GeneratedCode = GeneratedCode {
    genIndexJs :: M.Map P.ModuleName String
  , genExterns :: M.Map P.ModuleName String
}

-- | Make actions that compile in memory
inMemoryMakeActions :: M.Map P.ModuleName (FilePath, P.ForeignJS) -> IORef GeneratedCode -> P.MakeActions P.Make
inMemoryMakeActions foreigns genCodeRef = P.MakeActions getInputTimestamp getOutputTimestamp readExterns codegen progress
    where
        getInputTimestamp _ = return $ Left P.RebuildAlways
        getOutputTimestamp _ = return $ Nothing
        progress = liftIO . hPutStrLn stderr
        readExterns mn = liftIO $ do
            genCode <- readIORef genCodeRef
            case M.lookup mn (genExterns genCode) of
                Just js -> return ("<extern for " ++ P.runModuleName mn ++ ">", js)
                Nothing -> error $ "Unable to find externs for " ++ P.runModuleName mn

        codegen :: CF.Module CF.Ann -> P.Environment -> P.SupplyVar -> P.Externs -> P.Make ()
        codegen m _ nextVar exts = do
            let mn = CF.moduleName m
            foreignInclude <- case mn `M.lookup` foreigns of
              Just _ -> return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
              Nothing -> return Nothing
            pjs <- P.evalSupplyT nextVar $ P.prettyPrintJS <$> J.moduleToJs m foreignInclude

            liftIO $ atomicModifyIORef' genCodeRef $ \genCode2 ->
                let newIndex = M.insert mn pjs $ genIndexJs genCode2
                    newExtern = M.insert mn exts $ genExterns genCode2
                 in (GeneratedCode newIndex newExtern, ())

-- | Compile for production
compileProd :: ParseOutput -> IO CompilerOutput
compileProd (ms, foreigns) = do
    genRef <- newIORef $ GeneratedCode M.empty M.empty
    let makeActions = inMemoryMakeActions foreigns genRef
    e <- P.runMake P.defaultOptions $ P.make makeActions ms
    case e of
        Left err -> do
            hPutStrLn stderr $ P.prettyPrintMultipleErrors False err
            error "Error compiling purescript"
        Right (_, warnings') -> do
            when (P.nonEmpty warnings') $
                hPutStrLn stderr $ P.prettyPrintMultipleWarnings False warnings'
            genCode <- readIORef genRef
            return [(B.ModuleIdentifier (P.runModuleName mn) B.Regular, js) | (mn, js) <- M.toList $ genIndexJs genCode ]

-- | Bundle the generated javascript
bundle :: PsGeneratorOptions -> [String] -> ParseOutput -> CompilerOutput -> IO (Either BL.ByteString BL.ByteString)
bundle opts srcNames (ms, foreigns) indexJs = do
    let checkSrcMod (Left _, _) = Nothing
        checkSrcMod (Right fp, P.Module _ mn _ _)
            | fp `elem` srcNames = Just mn
            | otherwise = Nothing
        srcModNames = catMaybes $ map checkSrcMod ms

        roots = case psDeadCodeElim opts of
                    AllSourceModules -> [ B.ModuleIdentifier (P.runModuleName mn) B.Regular | mn <- srcModNames]
                    SpecifiedModules mods -> [ B.ModuleIdentifier mn B.Regular | mn <- mods]

    let foreignBundleInput = [(B.ModuleIdentifier (P.runModuleName mn) B.Foreign, js) | (mn, (_, js)) <- M.toList foreigns ]
    
    case B.bundle (indexJs ++ foreignBundleInput) roots Nothing "PS" of
        Left err -> do
            hPutStrLn stderr $ unlines $ B.printErrorMessage err
            return $ Left $ TL.encodeUtf8 $ TL.pack $ unlines $ B.printErrorMessage err
        Right r -> return $ Right $ TL.encodeUtf8 $ TL.pack r
