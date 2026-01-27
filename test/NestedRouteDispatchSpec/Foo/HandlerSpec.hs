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
        it "lets me do setUrl" $ do
            request $ do
                setUrl (WithParentArgs 1 FooIndexR)
                setUrlNested 1 FooIndexR
            bodyEquals "getFooIndexR: 1"
