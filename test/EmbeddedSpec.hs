{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module EmbeddedSpec (spec) where

import RunGenerator
import Data.Monoid
import Yesod.PureScript.EmbeddedGenerator
import Test.Hspec

genPurescript "pstest.js" PsGeneratorOptions
    { psSourceDirectory = "test/ps2"
    , psDependencySrcGlobs = ["test/ps1/**/*.purs"]
    , psDependencyForeignGlobs = ["test/ps1/**/*.js"]
    , psDeadCodeElim = AllSourceModules
    , psProductionMinimizer = \x -> return $ x <> "console.log(\"From minimizer!\");"
    , psDevBuildDirectory = "dev-build-dir"
    }

spec :: Spec
spec = describe "Spec for the EmbeddedStatic generator" $ do

    it "builds and executes purescript for production" $
        --writeFile "out.js" =<< pscode_prod
        execPurescript pstest_js_prod "console.log(PS.ModB.abcdef(\"prefix\"));\n"
            `shouldReturn` "From minimizer!\nprefixdefAbc\n"

    it "builds and executes purescript in development mode" $ do
        execPurescript pstest_js_dev "console.log(PS.ModB.abcdef(\"dev\"));\n"
            `shouldReturn` "devdefAbc\n"

        -- a second time should not build anything.  At the moment we can't actually
        -- test that but you can see it in the console output of the test itself.
        execPurescript pstest_js_dev "console.log(PS.ModB.abcdef(\"dev\"));\n"
            `shouldReturn` "devdefAbc\n"
