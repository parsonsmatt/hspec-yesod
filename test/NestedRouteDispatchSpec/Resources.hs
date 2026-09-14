{-# language QuasiQuotes #-}

module NestedRouteDispatchSpec.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types
import Data.IORef

data Event
    = Middleware
    | LegacyAuthorizing
    | NamedAuthorizing
    | Authorizing
    | Handling
    | MiddlewareFinished
    | ErrorRendering
    deriving (Eq, Show)

newtype App = App { appEvents :: IORef [Event] }

newApp :: IO App
newApp = App <$> newIORef []

recordEvent :: Event -> HandlerFor App ()
recordEvent event = do
    App events <- getYesod
    liftIO $ modifyIORef' events (++ [event])

resources :: [ResourceTree String]
resources = [parseRoutesNoCheck|

/   HomeR GET
/mount/#Int MountR AuthSub getRootSub

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
