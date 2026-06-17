module NestedRouteDispatchSpec.Foo.HandlerSpec where

import NestedRouteDispatchSpec.Foo.Handler () -- need YesodDispatchNested FooR instance
import NestedRouteDispatchSpec.Resources (App(..))
import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.YesodData () -- need Yesod App instance
import Yesod.Core (WithParentArgs(..))
import Test.Hspec.Yesod (request, bodyEquals, siteToYesodExampleData, setUrl, setUrlNested)
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
