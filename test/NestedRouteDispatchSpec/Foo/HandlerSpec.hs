{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NestedRouteDispatchSpec.Foo.HandlerSpec where

import Control.Monad (forM_)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.IORef
import qualified Data.Map as Map
import NestedRouteDispatchSpec.Assertions
import NestedRouteDispatchSpec.Foo.Handler () -- need YesodDispatchNested FooR instance
import NestedRouteDispatchSpec.Resources (App(..), Event(..), newApp)
import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.Subsite.Route
import NestedRouteDispatchSpec.YesodData () -- need Yesod App instance
import Yesod.Core (WithParentArgs(..), RedirectUrl(..), UrlToDispatch(..), toWaiAppYreNested, liftIO)
import qualified Test.Hspec as Hspec
import Test.Hspec.Yesod
import Test.Hspec (Spec, before, it)

-- Request an unmatched path through the same fragment dispatcher. Text URLs
-- would require whole-site dispatch, which this module must not import.
newtype FooPath = FooPath Text

instance RedirectUrl App FooPath where
    toTextUrl (FooPath path) = pure path

instance UrlToDispatch FooPath App where
    urlToDispatch _ = toWaiAppYreNested (Proxy :: Proxy FooR) 1

spec :: Spec
spec = do
    before (siteToYesodExampleData <$> newApp) $ do
        it "lets me dispatch a nested route fragment via setUrl" $ do
            request $ setUrl (WithParentArgs 1 FooIndexR)
            statusIs 200
            bodyEquals "getFooIndexR: 1"
            eventsShouldBe authorizedEvents
        it "lets me dispatch a nested route fragment via setUrlNested" $ do
            request $ setUrlNested 1 FooIndexR
            statusIs 200
            bodyEquals "getFooIndexR: 1"
            eventsShouldBe authorizedEvents

        it "authorizes a typed fragment through get without whole-site dispatch" $ do
            get (WithParentArgs 1 (FooShowR 2))
            statusIs 200
            bodyEquals "getFooShowR: (1,2)"
            eventsShouldBe authorizedEvents

        it "passes parent arguments to authorization" $ do
            get (WithParentArgs 3 FooIndexR)
            statusIs 403
            bodyContains "Wrong parent"
            eventsShouldBe wrapperDeniedEvents

        it "passes the matched leaf argument to authorization" $ do
            get (WithParentArgs 1 (FooShowR 3))
            statusIs 403
            bodyContains "Wrong item"
            eventsShouldBe wrapperDeniedEvents

        it "denies writes before running the handler" $ do
            post (WithParentArgs 1 FooIndexR)
            statusIs 403
            bodyContains "Writes require permission"
            eventsShouldBe wrapperDeniedEvents

        it "passes request headers to the authorizer" $ do
            request $ do
                setMethod "POST"
                setUrlNested 1 FooIndexR
                addRequestHeader ("X-Allow-Write", "yes")
            statusIs 200
            bodyEquals "postFooIndexR: 1"
            eventsShouldBe authorizedEvents

        it "denies an unauthorized method mismatch before reporting 405" $ do
            request $ do
                setMethod "DELETE"
                setUrl (WithParentArgs 1 FooIndexR)
            statusIs 403
            eventsShouldBe wrapperDeniedEvents

        it "reports 405 after successful authorization of an unsupported method" $ do
            request $ do
                setMethod "DELETE"
                setUrlNested 1 FooIndexR
                addRequestHeader ("X-Allow-Write", "yes")
            statusIs 405
            eventsShouldBe wrapperDeniedEvents

        it "passes trailing path pieces to the authorizer and handler" $ do
            get (WithParentArgs 1 (FooFilesR ["one", "two"]))
            statusIs 200
            bodyEquals "one/two"
            eventsShouldBe authorizedEvents

        it "denies trailing path pieces that fail authorization" $ do
            get (WithParentArgs 1 (FooFilesR ["private"]))
            statusIs 403
            bodyContains "Wrong files"
            eventsShouldBe wrapperDeniedEvents

        it "reports authentication failure without running the handler" $ do
            request $ do
                setUrlNested 1 FooLoginRequiredR
                addRequestHeader ("Accept", "application/json")
            statusIs 401
            eventsShouldBe wrapperDeniedEvents

        it "reevaluates authorization when a later request changes parent arguments" $ do
            get (WithParentArgs 1 FooIndexR)
            statusIs 200
            get (WithParentArgs 3 FooIndexR)
            statusIs 403
            eventsShouldBe (authorizedEvents ++ wrapperDeniedEvents)

        it "passes captures and the matched fragment to named subtree authorization" $ do
            get (WithParentArgs 1 (FooShowR 2))
            statusIs 200
            assertHeader "X-Named-Route" "(1,FooShowR 2)"
            eventsShouldBe authorizedEvents

        it "keeps legacy authorization ahead of both opt-in checks" $ do
            request $ do
                setUrlNested 1 FooLoginRequiredR
                addRequestHeader ("X-Deny-Legacy", "yes")
                addRequestHeader ("X-Deny-Named", "yes")
            statusIs 403
            bodyContains "Legacy denied"
            eventsShouldBe [Middleware, LegacyAuthorizing, MiddlewareFinished, ErrorRendering]

        forM_ ["GET", "DELETE"] $ \method ->
            it ("denies in named authorization before the wrapper or 405 for " ++ show method) $ do
                request $ do
                    setUrlNested 1 FooLoginRequiredR
                    setMethod method
                    addRequestHeader ("X-Deny-Named", "yes")
                statusIs 403
                bodyContains "Named denied"
                eventsShouldBe namedDeniedEvents

        it "preserves named authentication redirects for HTML requests" $ do
            request $ do
                setUrlNested 1 FooIndexR
                addRequestHeader ("X-Deny-Named", "login")
                addRequestHeader ("Accept", "text/html")
            statusIs 303
            assertHeader "Location" "/"
            eventsShouldBe [Middleware, LegacyAuthorizing, NamedAuthorizing, MiddlewareFinished]
            site <- getTestYesod
            liftIO $ Map.lookup "_ULT" <$> readIORef (appSession site)
                `Hspec.shouldReturn` Just "/foo/1"

        it "preserves named authentication failures for JSON requests" $ do
            site <- getTestYesod
            liftIO $ writeIORef (appSession site) (Map.singleton "_ULT" "/previous")
            request $ do
                setUrlNested 1 FooIndexR
                addRequestHeader ("X-Deny-Named", "login")
                addRequestHeader ("Accept", "application/json")
            statusIs 401
            eventsShouldBe namedDeniedEvents
            liftIO $ Map.lookup "_ULT" <$> readIORef (appSession site)
                `Hspec.shouldReturn` Just "/previous"

        it "finishes middleware and renders handler errors without reauthorizing" $ do
            get (WithParentArgs 1 FooErrorR)
            statusIs 400
            bodyContains "Handler failed"
            eventsShouldBe (authorizedEvents ++ [ErrorRendering])

        it "does not authorize an unmatched path in the fragment" $ do
            get (FooPath "/foo/1/not-an-item")
            statusIs 404
            eventsShouldBe [Middleware, MiddlewareFinished, ErrorRendering]

        it "does not cache a denial across later authorized requests" $ do
            get (WithParentArgs 3 FooIndexR)
            statusIs 403
            get (WithParentArgs 1 FooIndexR)
            statusIs 200
            eventsShouldBe (wrapperDeniedEvents ++ authorizedEvents)

        it "reevaluates named authorization when request headers change" $ do
            request $ do
                setUrlNested 1 FooIndexR
                addRequestHeader ("X-Deny-Named", "yes")
            statusIs 403
            get (WithParentArgs 1 FooIndexR)
            statusIs 200
            eventsShouldBe (namedDeniedEvents ++ authorizedEvents)

        it "authorizes a subsite mount reached through the fragment dispatcher" $ do
            get (WithParentArgs 1 (FooMountR 2 PageR))
            statusIs 200
            bodyEquals "subsite page"
            eventsShouldBe [Middleware, LegacyAuthorizing, NamedAuthorizing, Handling, MiddlewareFinished]

        forM_ [(3, 2), (1, 3)] $ \(parent, mount) ->
            it ("passes parent and mount captures to authorization: " ++ show (parent, mount)) $ do
                get (WithParentArgs parent (FooMountR mount PageR))
                statusIs 403
                bodyContains "Mount denied"
                eventsShouldBe namedDeniedEvents

        it "retains mount authorization through a second subsite" $ do
            get (WithParentArgs 1 (FooMountR 2 (DeepR LeafR)))
            statusIs 200
            bodyEquals "deep subsite"
            eventsShouldBe [Middleware, LegacyAuthorizing, NamedAuthorizing, Handling, MiddlewareFinished]

        it "denies access to a second subsite before its handler runs" $ do
            get (WithParentArgs 1 (FooMountR 3 (DeepR LeafR)))
            statusIs 403
            eventsShouldBe namedDeniedEvents

        it "honors the site's custom write classification at a mount" $ do
            request $ do
                setUrlNested 1 (FooMountR 2 PageR)
                addRequestHeader ("X-Treat-As-Write", "yes")
            statusIs 403
            eventsShouldBe namedDeniedEvents

        it "denies an unauthorized method at a mount before reporting 405" $ do
            request $ do
                setUrlNested 1 (FooMountR 2 PageR)
                setMethod "DELETE"
            statusIs 403
            eventsShouldBe namedDeniedEvents

        it "reports a subsite 405 only after mount authorization succeeds" $ do
            request $ do
                setUrlNested 1 (FooMountR 2 PageR)
                setMethod "DELETE"
                addRequestHeader ("X-Allow-Write", "yes")
            statusIs 405
            eventsShouldBe namedDeniedEvents

        it "authorizes an unmatched subsite path before returning 404" $ do
            get (FooPath "/foo/1/mount/2/missing")
            statusIs 404
            eventsShouldBe [Middleware, NamedAuthorizing, MiddlewareFinished, ErrorRendering]

        it "denies an unmatched path inside a forbidden mount" $ do
            get (FooPath "/foo/1/mount/3/missing")
            statusIs 403
            eventsShouldBe [Middleware, NamedAuthorizing, MiddlewareFinished, ErrorRendering]

        forM_ [("GET", 404), ("HEAD", 404), ("OPTIONS", 404), ("TRACE", 404),
               ("POST", 403), ("PUT", 403), ("PATCH", 403), ("DELETE", 403)] $ \(method, status) ->
            it ("uses default write classification on an unmatched subsite path for " ++ show method) $ do
                request $ do
                    setUrl (FooPath "/foo/1/mount/2/missing")
                    setMethod method
                    -- A missing route cannot use the site's override.
                    addRequestHeader ("X-Treat-As-Write", "yes")
                statusIs status
                eventsShouldBe [Middleware, NamedAuthorizing, MiddlewareFinished, ErrorRendering]

    before (siteToYesodExampleData . (\site -> site { appLoginEnabled = False }) <$> newApp) $
        forM_ ["text/html", "application/json"] $ \accept ->
            it ("denies named authentication without a login route for " ++ show accept) $ do
                site <- getTestYesod
                liftIO $ writeIORef (appSession site) (Map.singleton "_ULT" "/previous")
                request $ do
                    setUrlNested 1 FooIndexR
                    addRequestHeader ("X-Deny-Named", "login")
                    addRequestHeader ("Accept", accept)
                statusIs 401
                eventsShouldBe namedDeniedEvents
                liftIO $ Map.lookup "_ULT" <$> readIORef (appSession site)
                    `Hspec.shouldReturn` Just "/previous"
