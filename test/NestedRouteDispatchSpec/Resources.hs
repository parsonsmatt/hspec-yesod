{-# language QuasiQuotes #-}

module NestedRouteDispatchSpec.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types
import Data.IORef
import Yesod.Core.Types (SessionMap)

data Event
    = Middleware
    | LegacyAuthorizing
    | NamedAuthorizing
    | Authorizing
    | Handling
    | MiddlewareFinished
    | ErrorRendering
    deriving (Eq, Show)

data App = App
    { appEvents :: IORef [Event]
    , appSession :: IORef SessionMap
    , appLoginEnabled :: Bool
    }

newApp :: IO App
newApp = App <$> newIORef [] <*> newIORef mempty <*> pure True

recordEvent :: Event -> HandlerFor App ()
recordEvent event = do
    events <- appEvents <$> getYesod
    liftIO $ modifyIORef' events (++ [event])

resources :: [ResourceTree String]
resources = [parseRoutesNoCheck|

/   HomeR GET
/mount/#Int MountR AuthSub getRootSub
/static StaticR:
    /leaf StaticLeafR GET POST

/foo/#Int   FooR:
    /       FooIndexR   GET POST
    /edit   FooEditR    GET
    /files/*Texts FooFilesR GET
    /login-required FooLoginRequiredR GET
    /error FooErrorR GET
    /mount/#Int FooMountR AuthSub getFooSub
    /#Int   FooShowR    GET

/org/#Int OrgR:
    /account/#Text AccountR:
        /item/#Int AccountItemR GET

|]

nestDefaultOptsFor :: String -> RouteOpts
nestDefaultOptsFor target =
    setFocusOnNestedRoute target nestDefaultOpts

nestDefaultOpts :: RouteOpts
nestDefaultOpts =
    setNestedRouteFallthrough True defaultOpts
