{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.Subsite.Handler where

import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.Subsite.Route
import Yesod.Core
import Data.Text (Text)
import qualified Network.HTTP.Types as H
import Network.Wai (responseLBS)

-- The group delegates through YesodSubDispatchNested; /wai below exercises
-- subTopDispatch. Both must retain the outer mount's named parent runner.
mkNestedSubDispatchInstance defaultOpts "GroupR" [] NoTyArgs pure resourcesAuthSub

instance YesodSubDispatch LeafSub App where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesLeafSub)

instance YesodSubDispatch AuthSub App where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuthSub)

getLeafSub :: AuthSub -> LeafSub
getLeafSub _ = LeafSub

getPageR :: SubHandlerFor AuthSub App Text
getPageR = liftHandler $ recordEvent Handling >> pure "subsite page"

getWritableR, deleteWritableR :: SubHandlerFor AuthSub App Text
getWritableR = getPageR
deleteWritableR = getPageR

getAuthWai :: AuthSub -> WaiSubsiteWithAuth
getAuthWai _ = WaiSubsiteWithAuth $ \_ respond ->
    respond $ responseLBS H.status200 [] "guarded WAI"

getLeafR :: SubHandlerFor LeafSub App Text
getLeafR = liftHandler $ recordEvent Handling >> pure "deep subsite"
