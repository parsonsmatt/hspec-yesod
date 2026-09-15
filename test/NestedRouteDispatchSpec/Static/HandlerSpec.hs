{-# LANGUAGE OverloadedStrings #-}

module NestedRouteDispatchSpec.Static.HandlerSpec (spec) where

import Control.Monad (forM_)
import NestedRouteDispatchSpec.Assertions
import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.Static.Handler ()
import NestedRouteDispatchSpec.Static.Route
import NestedRouteDispatchSpec.YesodData ()
import Test.Hspec (Spec, before, it)
import Test.Hspec.Yesod
import Yesod.Core (WithParentArgs(..))

spec :: Spec
spec = before (siteToYesodExampleData <$> newApp) $ do
    it "dispatches a bare fragment with no parent captures and authorizes it" $ do
        get StaticLeafR
        statusIs 200
        bodyEquals "static read"
        eventsShouldBe authorizedEvents

    it "accepts explicit unit parent arguments for the same fragment" $ do
        request $ setUrl (WithParentArgs () StaticLeafR)
        statusIs 200
        eventsShouldBe authorizedEvents

    forM_ ["POST", "DELETE"] $ \method ->
        it ("denies a static fragment before its handler or 405 for " ++ show method) $ do
            request $ setUrl StaticLeafR >> setMethod method
            statusIs 403
            bodyContains "Static write denied"
            eventsShouldBe namedDeniedEvents

    it "authorizes a static Html handler through the concrete wrapper" $ do
        request $ do
            setUrlNested () StaticLeafR
            setMethod "POST"
            addRequestHeader ("X-Allow-Write", "yes")
        statusIs 200
        bodyEquals "static write"
        eventsShouldBe authorizedEvents

    it "authorizes a static 405 through the concrete wrapper" $ do
        request $ do
            setUrl StaticLeafR
            setMethod "DELETE"
            addRequestHeader ("X-Allow-Write", "yes")
        statusIs 405
        eventsShouldBe wrapperDeniedEvents
