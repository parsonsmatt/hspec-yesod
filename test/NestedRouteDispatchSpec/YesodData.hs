{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.YesodData where

import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.Resources
import Yesod.Core

mkYesodDataOpts nestDefaultOpts "App" resources

-- No authorization instances or handlers are imported by the foundation.
instance Yesod App where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing
    yesodMiddleware handler = do
        recordEvent Middleware
        defaultYesodMiddleware handler
