{-# LANGUAGE OverloadedStrings #-}

module NestedRouteDispatchSpec.Account.HandlerSpec (spec) where

import Control.Monad (forM_)
import NestedRouteDispatchSpec.Account.Handler ()
import NestedRouteDispatchSpec.Account.Route
import NestedRouteDispatchSpec.Assertions
import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.YesodData ()
import Test.Hspec (Spec, before, it)
import Test.Hspec.Yesod
import Yesod.Core (WithParentArgs(..))

-- Neither Foo.Handler nor YesodDispatch App is imported, even transitively.
spec :: Spec
spec = before (siteToYesodExampleData <$> newApp) $ do
    it "dispatches with a tuple of parent arguments and only its own authorizer" $ do
        get (WithParentArgs (1, "alice") (AccountItemR 2))
        statusIs 200
        bodyEquals "account item"
        eventsShouldBe [Middleware, LegacyAuthorizing, Authorizing, Handling, MiddlewareFinished]

    it "accepts tuple parent arguments through setUrlNested" $ do
        request $ setUrlNested (1, "alice") (AccountItemR 2)
        statusIs 200
        eventsShouldBe [Middleware, LegacyAuthorizing, Authorizing, Handling, MiddlewareFinished]

    forM_ [(9, "alice", 2), (1, "bob", 2), (1, "alice", 9)] $ \(org, account, item) ->
        it ("passes all captures to its authorizer: " ++ show (org, account, item)) $ do
            get (WithParentArgs (org, account) (AccountItemR item))
            statusIs 403
            bodyContains "Account denied"
            eventsShouldBe [Middleware, LegacyAuthorizing, Authorizing, MiddlewareFinished, ErrorRendering]

    it "runs the concrete TypedContent wrapper around a nested 405" $ do
        request $ do
            setUrlNested (1, "alice") (AccountItemR 2)
            setMethod "DELETE"
        statusIs 405
        eventsShouldBe [Middleware, LegacyAuthorizing, Authorizing, MiddlewareFinished, ErrorRendering]

    it "reports denial before a nested 405 for wrong tuple captures" $ do
        request $ do
            setUrlNested (1, "bob") (AccountItemR 2)
            setMethod "DELETE"
        statusIs 403
        eventsShouldBe [Middleware, LegacyAuthorizing, Authorizing, MiddlewareFinished, ErrorRendering]

    it "reevaluates the tuple when requests select different parent arguments" $ do
        get (WithParentArgs (1, "alice") (AccountItemR 2))
        statusIs 200
        get (WithParentArgs (1, "bob") (AccountItemR 2))
        statusIs 403
        eventsShouldBe
            [ Middleware, LegacyAuthorizing, Authorizing, Handling, MiddlewareFinished
            , Middleware, LegacyAuthorizing, Authorizing, MiddlewareFinished, ErrorRendering ]
