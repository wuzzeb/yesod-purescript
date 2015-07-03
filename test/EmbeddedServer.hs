{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}
module EmbeddedServer (main) where

import Yesod.Core
import Yesod.EmbeddedStatic
import StaticFiles

data MyApp = MyApp { getStatic :: EmbeddedStatic }

mkYesod "MyApp" [parseRoutes|
/ HomeR GET
/static StaticR EmbeddedStatic getStatic
|]

instance Yesod MyApp

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    addScript $ StaticR js_pure_js
    [whamlet|<p>Hello World!|]

main :: IO ()
main = warp 3000 $ MyApp myStatic
