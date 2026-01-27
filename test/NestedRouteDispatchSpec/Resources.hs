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

mkRouteOpts :: Maybe String -> RouteOpts
mkRouteOpts mtarget =
    setFocusOnNestedRoute mtarget $
        setNestedRouteFallthrough True $
            defaultOpts
