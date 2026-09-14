{-# language QuasiQuotes #-}

module NestedRouteDispatchSpec.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types
import Data.IORef

data Event = Middleware | Authorizing | Handling
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

/foo/#Int   FooR:
    /       FooIndexR   GET POST
    /edit   FooEditR    GET
    /files/*Texts FooFilesR GET
    /login-required FooLoginRequiredR GET
    /#Int   FooShowR    GET

|]

nestDefaultOptsFor :: String -> RouteOpts
nestDefaultOptsFor target =
    setFocusOnNestedRoute target nestDefaultOpts

nestDefaultOpts :: RouteOpts
nestDefaultOpts =
    setNestedRouteFallthrough True defaultOpts
