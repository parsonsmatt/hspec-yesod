{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Only WholeSiteSpec imports this dispatcher. Fragment specs deliberately
-- compile without it or their sibling's authorizers.
module NestedRouteDispatchSpec.YesodDispatch where

import NestedRouteDispatchSpec.Foo.Handler (authorizeFooMountR)
import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.Account.Handler ()
import NestedRouteDispatchSpec.Account.Route (AccountR(..))
import NestedRouteDispatchSpec.Static.Handler ()
import NestedRouteDispatchSpec.Static.Route (StaticR(..))
import NestedRouteDispatchSpec.Authorization (routeAuthOpts)
import NestedRouteDispatchSpec.Subsite.Route
import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.YesodData
import Yesod.Core
import Data.Text (Text)

-- There is no Authorize (Route App) instance. Clearing the shared wrapper
-- must preserve named authorization and the wrappers of delegated fragments.
mkYesodDispatchOpts
    (unsetRouteHandlerWrapper $ routeAuthOpts $ setRouteAuthorization RouteAuthPerResource nestDefaultOpts)
    "App" resources

authorizeHomeR :: RouteAuthorizer App
authorizeHomeR = RouteAuthorizer $ \_ -> do
    recordEvent NamedAuthorizing
    deny <- lookupHeader "X-Deny-Named"
    pure $ if deny == Just "yes" then Unauthorized "Root denied" else Authorized

authorizeMountR :: Int -> RouteAuthorizer App
authorizeMountR = authorizeFooMountR 1

getRootSub :: App -> Int -> AuthSub
getRootSub _ _ = AuthSub

getHomeR :: HandlerFor App Text
getHomeR = recordEvent Handling >> pure "HomeR"
