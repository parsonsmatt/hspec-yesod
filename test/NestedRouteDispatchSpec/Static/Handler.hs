{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.Static.Handler where

import Data.Text (Text)
import NestedRouteDispatchSpec.Authorization
import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.Static.Route
import Yesod.Core hiding (isAuthorized)

instance Authorize StaticR where
    isAuthorized (WithParentArgs () StaticLeafR) =
        recordEvent Authorizing >> pure (Allowed "static")

mkYesodDispatchOpts
    (routeAuthOpts $ setRouteAuthorization RouteAuthPerResource $ nestDefaultOptsFor "StaticR")
    "App" resources

authorizeStaticLeafR :: Bool -> HandlerFor App AuthResult
authorizeStaticLeafR isWrite = do
    recordEvent NamedAuthorizing
    permission <- lookupHeader "X-Allow-Write"
    pure $ if not isWrite || permission == Just "yes"
        then Authorized else Unauthorized "Static write denied"

getStaticLeafR :: HandlerFor App Text
getStaticLeafR = recordEvent Handling >> pure "static read"

postStaticLeafR :: HandlerFor App Html
postStaticLeafR = recordEvent Handling >> pure (toHtml ("static write" :: Text))
