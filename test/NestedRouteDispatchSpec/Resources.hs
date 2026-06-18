{-# language QuasiQuotes #-}

module NestedRouteDispatchSpec.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types

data App = App

resources :: [ResourceTree String]
resources = [parseRoutesNoCheck|

/   HomeR GET

/foo/#Int   FooR:
    /       FooIndexR   GET
    /edit   FooEditR    GET
    /#Int   FooShowR    GET

|]

nestDefaultOptsFor :: String -> RouteOpts
nestDefaultOptsFor target =
    setFocusOnNestedRoute target nestDefaultOpts

nestDefaultOpts :: RouteOpts
nestDefaultOpts =
    setNestedRouteFallthrough True defaultOpts
