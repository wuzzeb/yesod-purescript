{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module RunGenerator where

import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Process (readProcess)
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Types
import Yesod.PureScript.EmbeddedGenerator

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- | Run the purescript generator with the given options.  Will create two definitions with a name
-- based on the location.
--
-- > location_prod :: IO String
-- > location_dev :: IO String
genPurescript :: Location -> PsGeneratorOptions -> Q [Dec]
genPurescript loc opts = do
    [g] <- purescript loc opts
    let name = fromMaybe (error "No name!") $ ebHaskellName g

    ioStringT <- [t|IO String|]

    let prodName = mkName $ showName name ++ "_prod"
    prodBs <- runIO $ ebProductionContent g
    let prodExpr = [| return $(liftString $ TL.unpack $ TL.decodeUtf8 prodBs) |]
    prodDec <- valD (varP prodName) (normalB prodExpr) []

    let devName = mkName $ showName name ++ "_dev"
    let devExpr = [| (TL.unpack . TL.decodeUtf8) <$> $(ebDevelReload g) |]
    devDec <- valD (varP devName) (normalB devExpr) []
    
    return
        [ SigD prodName ioStringT, prodDec
        , SigD devName ioStringT, devDec
        ]

-- | Executes purescript code.
execPurescript :: IO String -- ^ the purescript output from 'genPurescript'
               -> String -- ^ Javascript code to append, this should be JS code to call the actual test function
               -> IO String -- ^ returns the console output of the script
execPurescript genJs extraJs = do
    js <- genJs
    readProcess "node" [] $ concat [js, "\n", extraJs, "\n"]
