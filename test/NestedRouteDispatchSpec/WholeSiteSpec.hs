{-# LANGUAGE OverloadedStrings #-}

module NestedRouteDispatchSpec.WholeSiteSpec (spec) where

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Map as Map
import NestedRouteDispatchSpec.Assertions
import NestedRouteDispatchSpec.Foo.Route
import NestedRouteDispatchSpec.Account.Route
import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.Subsite.Route
import NestedRouteDispatchSpec.YesodData
import NestedRouteDispatchSpec.YesodDispatch ()
import Test.Hspec (Spec, before, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Yesod
import Yesod.Core (liftIO)

spec :: Spec
spec = before (siteToYesodExampleData <$> newApp) $ do
    it "clears the shared root wrapper without requiring a root authorization class instance" $ do
        get HomeR
        statusIs 200
        bodyEquals "HomeR"
        eventsShouldBe [Middleware, LegacyAuthorizing, NamedAuthorizing, Handling, MiddlewareFinished]

    it "retains named authorization after clearing the root wrapper" $ do
        request $ do
            setUrl HomeR
            addRequestHeader ("X-Deny-Named", "yes")
        statusIs 403
        bodyContains "Root denied"
        eventsShouldBe namedDeniedEvents

    it "retains a delegated fragment's wrapper when the root wrapper is cleared" $ do
        get (FooR 1 FooIndexR)
        statusIs 200
        eventsShouldBe authorizedEvents

    it "retains delegated authorization failures through whole-site dispatch" $ do
        get (FooR 3 FooIndexR)
        statusIs 403
        eventsShouldBe wrapperDeniedEvents

    it "uses the delegated fragment's own named policy setting" $ do
        -- Account's splice has only the class wrapper, even though this root
        -- splice enables RouteAuthPerResource. No authorizeAccountItemR exists.
        get (OrgR 1 (AccountR "alice" (AccountItemR 2)))
        statusIs 200
        eventsShouldBe [Middleware, LegacyAuthorizing, Authorizing, Handling, MiddlewareFinished]

    it "retains a separately compiled wrapper's denial under a named parent policy" $ do
        get (OrgR 9 (AccountR "alice" (AccountItemR 2)))
        statusIs 403
        eventsShouldBe [Middleware, LegacyAuthorizing, Authorizing, MiddlewareFinished, ErrorRendering]

    it "authorizes a flat subsite mount" $ do
        get (MountR 2 PageR)
        statusIs 200
        bodyEquals "subsite page"
        eventsShouldBe [Middleware, LegacyAuthorizing, NamedAuthorizing, Handling, MiddlewareFinished]

    it "denies a flat subsite mount before its handler" $ do
        get (MountR 3 PageR)
        statusIs 403
        eventsShouldBe namedDeniedEvents

    forM_ [Nothing, Just "yes"] $ \permission ->
        it ("authorizes flat mount method mismatches with permission " ++ show permission) $ do
            request $ do
                setUrl (MountR 2 PageR)
                setMethod "DELETE"
                forM_ permission $ \value -> addRequestHeader ("X-Allow-Write", value)
            statusIs $ maybe 403 (const 405) permission
            eventsShouldBe namedDeniedEvents

    it "retains flat mount authorization through a second subsite" $ do
        get (MountR 3 (DeepR LeafR))
        statusIs 403
        eventsShouldBe namedDeniedEvents

    forM_ [("writable", 200), ("missing", 403)] $ \(suffix, status) ->
        it ("uses a read override only on matched flat subsite routes: " ++ suffix) $ do
            request $ do
                setUrl ("/mount/2/" ++ suffix)
                setMethod "DELETE"
                addRequestHeader ("X-Treat-As-Read", "yes")
            statusIs status
            eventsShouldBe $ if status == 200
                then [Middleware, LegacyAuthorizing, NamedAuthorizing, Handling, MiddlewareFinished]
                else [Middleware, NamedAuthorizing, MiddlewareFinished, ErrorRendering]

    forM_ ["/wai/x", "/group/wai/x"] $ \suffix ->
        forM_ [(2, 200), (3, 403)] $ \(mount, status) ->
            it ("authorizes transitive WAI through a flat mount: " ++ show (suffix, mount)) $ do
                get ("/mount/" ++ show (mount :: Int) ++ suffix)
                statusIs status
                if status == 200 then bodyEquals "guarded WAI" else bodyContains "Mount denied"
                eventsShouldBe $ if status == 200
                    then [Middleware, LegacyAuthorizing, NamedAuthorizing, MiddlewareFinished]
                    else namedDeniedEvents

    forM_ ["text/html", "application/json"] $ \accept ->
        it ("enforces login on a flat subsite 404: " ++ show accept) $ do
            site <- getTestYesod
            liftIO $ writeIORef (appSession site) (Map.singleton "_ULT" "/previous")
            request $ do
                setUrl ("/mount/2/missing" :: String)
                addRequestHeader ("X-Mount-Login", "yes")
                addRequestHeader ("Accept", accept)
            statusIs (if accept == "text/html" then 303 else 401)
            eventsShouldBe $ [Middleware, NamedAuthorizing, MiddlewareFinished] ++
                [ErrorRendering | accept == "application/json"]
            liftIO $ Map.lookup "_ULT" <$> readIORef (appSession site)
                `Hspec.shouldReturn` Just "/previous"

    forM_ [("/mount/2/missing", 404), ("/mount/3/missing", 403)] $ \(path, status) ->
        it ("checks flat mount authorization on an unmatched subsite path: " ++ path) $ do
            get path
            statusIs status
            eventsShouldBe [Middleware, NamedAuthorizing, MiddlewareFinished, ErrorRendering]

    forM_ [("GET", 404), ("HEAD", 404), ("OPTIONS", 404), ("TRACE", 404),
           ("POST", 403), ("PUT", 403), ("PATCH", 403), ("DELETE", 403)] $ \(method, status) ->
        it ("uses default write classification on an unmatched flat subsite path for " ++ show method) $ do
            request $ do
                setUrl ("/mount/2/missing" :: String)
                setMethod method
                addRequestHeader ("X-Treat-As-Write", "yes")
            statusIs status
            eventsShouldBe [Middleware, NamedAuthorizing, MiddlewareFinished, ErrorRendering]

    it "does not invoke route authorizers on an unmatched whole-site path" $ do
        get ("/missing" :: String)
        statusIs 404
        eventsShouldBe [Middleware, MiddlewareFinished, ErrorRendering]
