
# yesod-purescript

Work in progress integration of PureScript (see https://www.purescript.org) into Yesod.


Goals:

- environment for easy PureScript development (with dev flag):
  - auto refresh: sources compiled on demand or when file changes, with cache,
  - convenient display of error messages.
- a way of embedding compiled PureScript into final app binary using
  TemplateHaskell.



## Install

1. Clone repo: `git clone https://github.com/mpietrzak/yesod-purescript.git`
2. Add source to cabal sandbox: `cd yout_project_dir && cabal sandbox add-source path_to_yesod_purescript_clone`.


## Usage

See https://github.com/mpietrzak/yesod-purescript-sample/.

The easiest way is to look at second commit in sample's repo:
https://github.com/mpietrzak/yesod-purescript-sample/commit/6e026b63deac97ac55c710e8fcf1f4e550a07bb3

To use yesod-purescript, you have to:

1. add yesod-purescript dependency to your project's cabal file,
2. add a field in app's data type for PureScriptSite and add instance of
   YesodPurescript typeclass for your app's data type,
3. add subsite route to PureScriptSite,
4. put your purs files in configured subdirectories of your project;
   by default those directories are:
   - `purs` - your code,
   - `bower_components` - dependencies (since most likely you'll use bower to get those).
5. Reference your code in HTML pages.


## Tutorial

See https://github.com/mpietrzak/yesod-purescript-sample/.

Rather verbose step by step instructions follow.


### 1. Dependency

Add `yesod-purescript` to `build-depends` in .cabal file.


### 2. App data and instance

Assuming you have basic project generated by running `yesod init`.

Go to `Foundation.hs`.

First add `Yesod.PureScrtipt` import to this file.

Notice fields in your App data like:

`appLogger :: Logger`

Add similar line for PureScript at the end:

`appPureScript :: PureScriptSite`.

Then add instance of YesodPureScript: `instance YesodPureScript App`.
You can add this after all other instances (order does not really matter).

Go to `Application.hs`:

- add import of `Yesod.PureScript`,
- find `makeFoundation` function,
  - add `appPureScript <- createYesodPureScriptSite def` close to top of this function.


### 3. Add subsite route

Add this line to `config/routes` after auth route definition:

`/purs PureScriptR PureScriptSite appPureScript`

This attaches Yesod PureScript Subsite at /purs.
Opening url http://localhost:3000/purs/Foo.js will try to
find and compile module Foo.

There's also status page at http://localhost:3000/purs which lists
loaded modules and module loading errors (if any).


### 4. Purs files

Put Hello.purs in purs subdirectory:

    module Hello (main)
    where

    import Control.Monad.Eff

    foreign import log
    """function log(s) {
        return function() {
            return window.console.log(s);
        };
    }""" :: forall eff. String -> Eff eff {}

    main = log "hello"

Create empty `bower_components` dir in your project dir.

Start dev server and open http://localhost:3000/purs/Hello.js.


### 5. Reference PureScript in HTML

Open Handler/Home.hs.
Add import: `import Yesod.PureScript (getPureScriptRoute)`.
Change getHomeR to reference your Hello.js script using addScript:

    getHomeR :: Handler Html
    getHomeR = do
        (formWidget, formEnctype) <- generateFormPost sampleForm
        let submission = Nothing :: Maybe (FileInfo, Text)
            handlerName = "getHomeR" :: Text
        defaultLayout $ do
            aDomId <- newIdent
            setTitle "Welcome To Yesod!"
            addScript $ PureScriptR $ getPureScriptRoute ["Hello"]
            $(widgetFile "homepage")


