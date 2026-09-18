{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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

/unrelated UnrelatedR:
    / UnrelatedHomeR GET

|]

nestDefaultOptsFor :: String -> RouteOpts
nestDefaultOptsFor target =
    setFocusOnNestedRoute target nestDefaultOpts

nestDefaultOpts :: RouteOpts
nestDefaultOpts =
    setRouteDispatchWrapper [t| AuthorizeRoute |]
        (\handler route -> [| let WithParentArgs args fragment = $route in authorizeRoute args fragment >> $handler |]) $
        setNestedRouteFallthrough True defaultOpts

class AuthorizeRoute route where
    authorizeRoute :: ParentArgs route -> route -> HandlerFor (ParentSite route) ()
