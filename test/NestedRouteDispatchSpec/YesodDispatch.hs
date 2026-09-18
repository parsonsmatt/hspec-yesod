{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is not imported by any spec on purpose. It builds the full
-- @YesodDispatch App@ instance (via 'mkYesodDispatchOpts' with
-- 'nestDefaultOpts'), and is kept in the test suite's @other-modules@ purely
-- to verify that the full application dispatch still compiles alongside the
-- nested route-fragment dispatch. Handler specs depend only on the cheap
-- @YesodDispatchNested FooR@ fragment; the real application still wants this
-- full @YesodDispatch@.
module NestedRouteDispatchSpec.YesodDispatch where

import NestedRouteDispatchSpec.Foo.Handler () -- needed for the yesod dispatch instance in scope
import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.YesodData
import Yesod.Core
import Data.Text (Text)

mkYesodDispatchOpts nestDefaultOpts "App" resources

getHomeR :: HandlerFor App Text
getHomeR = pure "HomeR"

-- The production assembly supplies policies that isolated Foo specs do not import.
instance AuthorizeRoute (Route App) where
    authorizeRoute () HomeR = pure ()
    authorizeRoute () (FooR parent route) = authorizeRoute parent route
    authorizeRoute () (UnrelatedR route) = authorizeRoute () route

instance AuthorizeRoute UnrelatedR where
    authorizeRoute () UnrelatedHomeR = permissionDenied "unrelated denied"

getUnrelatedHomeR :: HandlerFor App Text
getUnrelatedHomeR = pure "unrelated"
