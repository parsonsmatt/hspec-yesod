{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# language QuasiQuotes #-}

module NestedRouteDispatchSpec.Resources where

import Yesod.Core
import Yesod.Core.RouteLeaf
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
    setRouteLeafHandlerWrapper [t| AuthorizeRoute |]
        (\handler args leaf -> [| authorizeRoute $args $leaf >> $handler |]) $
        setNestedRouteFallthrough True defaultOpts

class HasRouteLeaves route => AuthorizeRoute route where
    authorizeRoute :: ParentArgs route -> RouteLeaves route -> HandlerFor (ParentSite route) ()
