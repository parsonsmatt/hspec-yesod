{-# LANGUAGE OverloadedStrings #-}

module NestedRouteDispatchSpec.Foo.HandlerSpec where

import Data.IORef (readIORef)
import NestedRouteDispatchSpec.Foo.Handler () -- need YesodDispatchNested FooR instance
import NestedRouteDispatchSpec.Resources (App(..), Event(..), newApp)
import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.YesodData () -- need Yesod App instance
import Yesod.Core (WithParentArgs(..), liftIO)
import Test.Hspec.Yesod
import Test.Hspec (Spec, before, it, shouldReturn)

eventsShouldBe :: [Event] -> YesodExample App ()
eventsShouldBe expected = do
    App events <- getTestYesod
    liftIO $ readIORef events `shouldReturn` expected

spec :: Spec
spec = do
    before (siteToYesodExampleData <$> newApp) $ do
        it "lets me dispatch a nested route fragment via setUrl" $ do
            request $ setUrl (WithParentArgs 1 FooIndexR)
            statusIs 200
            bodyEquals "getFooIndexR: 1"
            eventsShouldBe [Middleware, Authorizing, Handling]
        it "lets me dispatch a nested route fragment via setUrlNested" $ do
            request $ setUrlNested 1 FooIndexR
            statusIs 200
            bodyEquals "getFooIndexR: 1"
            eventsShouldBe [Middleware, Authorizing, Handling]

        it "authorizes a typed fragment through get without whole-site dispatch" $ do
            get (WithParentArgs 1 (FooShowR 2))
            statusIs 200
            bodyEquals "getFooShowR: (1,2)"
            eventsShouldBe [Middleware, Authorizing, Handling]

        it "passes parent arguments to authorization" $ do
            get (WithParentArgs 3 FooIndexR)
            statusIs 403
            bodyContains "Wrong parent"
            eventsShouldBe [Middleware, Authorizing]

        it "passes the matched leaf argument to authorization" $ do
            get (WithParentArgs 1 (FooShowR 3))
            statusIs 403
            bodyContains "Wrong item"
            eventsShouldBe [Middleware, Authorizing]

        it "denies writes before running the handler" $ do
            post (WithParentArgs 1 FooIndexR)
            statusIs 403
            bodyContains "Writes require permission"
            eventsShouldBe [Middleware, Authorizing]

        it "passes request headers to the authorizer" $ do
            request $ do
                setMethod "POST"
                setUrlNested 1 FooIndexR
                addRequestHeader ("X-Allow-Write", "yes")
            statusIs 200
            bodyEquals "postFooIndexR: 1"
            eventsShouldBe [Middleware, Authorizing, Handling]

        it "denies an unauthorized method mismatch before reporting 405" $ do
            request $ do
                setMethod "DELETE"
                setUrl (WithParentArgs 1 FooIndexR)
            statusIs 403
            eventsShouldBe [Middleware, Authorizing]

        it "reports 405 after successful authorization of an unsupported method" $ do
            request $ do
                setMethod "DELETE"
                setUrlNested 1 FooIndexR
                addRequestHeader ("X-Allow-Write", "yes")
            statusIs 405
            eventsShouldBe [Middleware, Authorizing]

        it "passes trailing path pieces to the authorizer and handler" $ do
            get (WithParentArgs 1 (FooFilesR ["one", "two"]))
            statusIs 200
            bodyEquals "one/two"
            eventsShouldBe [Middleware, Authorizing, Handling]

        it "denies trailing path pieces that fail authorization" $ do
            get (WithParentArgs 1 (FooFilesR ["private"]))
            statusIs 403
            bodyContains "Wrong files"
            eventsShouldBe [Middleware, Authorizing]

        it "reports authentication failure without running the handler" $ do
            request $ do
                setUrlNested 1 FooLoginRequiredR
                addRequestHeader ("Accept", "application/json")
            statusIs 401
            eventsShouldBe [Middleware, Authorizing]

        it "reevaluates authorization when a later request changes parent arguments" $ do
            get (WithParentArgs 1 FooIndexR)
            statusIs 200
            get (WithParentArgs 3 FooIndexR)
            statusIs 403
            eventsShouldBe [Middleware, Authorizing, Handling, Middleware, Authorizing]
