{-# LANGUAGE OverloadedStrings #-}
module NestedRouteDispatchSpec.Foo.HandlerSpec where

import NestedRouteDispatchSpec.Foo.Handler () -- need YesodDispatchNested FooR instance
import NestedRouteDispatchSpec.Resources (App(..))
import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.YesodData () -- need Yesod App instance
import Yesod.Core (WithParentArgs(..))
import Test.Hspec.Yesod (request, bodyEquals, siteToYesodExampleData, setUrl, setUrlNested, setMethod, statusIs)
import Test.Hspec (Spec, before, it)

spec :: Spec
spec = do
    before (pure (siteToYesodExampleData App)) $ do
        it "lets me dispatch a nested route fragment via setUrl" $ do
            request $ setUrl (WithParentArgs 1 FooIndexR)
            bodyEquals "getFooIndexR: 1"
        it "lets me dispatch a nested route fragment via setUrlNested" $ do
            request $ setUrlNested 1 FooIndexR
            bodyEquals "getFooIndexR: 1"

        it "rejects unauthorized parent captures" $ do
            request $ setUrlNested 0 FooIndexR
            statusIs 403
        it "rejects unauthorized leaf captures" $ do
            request $ setUrlNested 1 (FooShowR 0)
            statusIs 403
        it "accepts authorized parent and leaf captures" $ do
            request $ setUrlNested 1 (FooShowR 2)
            statusIs 200
            bodyEquals "getFooShowR: (1,2)"
        it "denies the route before running its handler" $ do
            request $ setUrlNested 1 FooEditR
            statusIs 403
        it "checks authorization before a matched-path method error" $ do
            request $ do
                setMethod "DELETE"
                setUrlNested 0 FooIndexR
            statusIs 403
        it "reports a method error after successful authorization" $ do
            request $ do
                setMethod "DELETE"
                setUrlNested 1 FooIndexR
            statusIs 405
