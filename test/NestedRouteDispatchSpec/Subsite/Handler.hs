{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.Subsite.Handler where

import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.Subsite.Route
import Yesod.Core
import Data.Text (Text)

instance YesodSubDispatch LeafSub App where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesLeafSub)

instance YesodSubDispatch AuthSub App where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuthSub)

getLeafSub :: AuthSub -> LeafSub
getLeafSub _ = LeafSub

getPageR :: SubHandlerFor AuthSub App Text
getPageR = liftHandler $ recordEvent Handling >> pure "subsite page"

getLeafR :: SubHandlerFor LeafSub App Text
getLeafR = liftHandler $ recordEvent Handling >> pure "deep subsite"
