{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.YesodData where

import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.Account.Route (AccountR(..))
import NestedRouteDispatchSpec.Subsite.Route
import NestedRouteDispatchSpec.Static.Route (StaticR(..))
import NestedRouteDispatchSpec.Resources
import qualified Network.Wai as W
import Data.Text (Text)
import Data.IORef
import UnliftIO.Exception (finally)
import Yesod.Core

mkYesodDataOpts nestDefaultOpts "App" resources

-- No authorization instances or handlers are imported by the foundation.
instance Yesod App where
    messageLoggerSource = mempty
    makeSessionBackend site = pure $ Just $ SessionBackend $ \_ -> do
        session <- readIORef (appSession site)
        pure (session, \saved -> writeIORef (appSession site) saved >> pure [])
    authRoute site = if appLoginEnabled site then Just HomeR else Nothing
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
