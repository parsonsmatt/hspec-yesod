{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.YesodData where

import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.Account.Route (AccountR(..))
import NestedRouteDispatchSpec.Subsite.Route
import NestedRouteDispatchSpec.Resources
import qualified Network.Wai as W
import Data.Text (Text)
import UnliftIO.Exception (finally)
import Yesod.Core

mkYesodDataOpts nestDefaultOpts "App" resources

-- No authorization instances or handlers are imported by the foundation.
instance Yesod App where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing
    authRoute _ = Just HomeR
    isAuthorized _ _ = do
        recordEvent LegacyAuthorizing
        deny <- lookupHeader "X-Deny-Legacy"
        pure $ if deny == Just "yes" then Unauthorized "Legacy denied" else Authorized
    isWriteRequest _ = do
        forceWrite <- lookupHeader "X-Treat-As-Write"
        method <- W.requestMethod <$> waiRequest
        pure $ forceWrite == Just "yes" || method `notElem` ["GET", "HEAD", "OPTIONS", "TRACE"]
    yesodMiddleware handler = do
        recordEvent Middleware
        defaultYesodMiddleware handler `finally` recordEvent MiddlewareFinished
    errorHandler err = recordEvent ErrorRendering >> defaultErrorHandler err
