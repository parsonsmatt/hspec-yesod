{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module NestedRouteDispatchSpec.InlineSpec (spec) where

import Control.Monad (forM_)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import NestedRouteDispatchSpec.Resources (Event(..))
import Test.Hspec (Spec, before, it, shouldReturn)
import Test.Hspec.Yesod
import UnliftIO.Exception (finally)
import Yesod.Core

-- No setParameterizedSubroute: the nested leaves must use inline
-- compatibility dispatch, while keeping subtree authorizer names/arguments.
data InlineApp a = InlineApp (IORef [Event])

mkYesodOpts
    (setRouteHandlerWrapper (\handler route -> [| typedWrapper $route $handler |]) $
        setRouteAuthorization RouteAuthSubtree defaultOpts)
    "InlineApp a" [parseRoutes|
/ RootR GET POST
/any AnyR
/static StaticR:
    /leaf StaticLeafR GET POST
/org/#Int OrgR:
    /account/#Text AccountR:
        /item/#Int ItemR GET POST
        /files/*Texts FilesR GET
|]

record :: Event -> HandlerFor (InlineApp a) ()
record event = do
    InlineApp ref <- getYesod
    liftIO $ modifyIORef' ref (++ [event])

instance Yesod (InlineApp a) where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing
    isAuthorized _ _ = record LegacyAuthorizing >> pure Authorized
    yesodMiddleware handler = do
        record Middleware
        defaultYesodMiddleware handler `finally` record MiddlewareFinished
    errorHandler err = record ErrorRendering >> defaultErrorHandler err

typedWrapper
    :: WithParentArgs (Route (InlineApp a))
    -> HandlerFor (InlineApp a) TypedContent
    -> HandlerFor (InlineApp a) TypedContent
typedWrapper (WithParentArgs () route) handler = do
    record Authorizing
    addHeader "X-Wrapper-Route" (Text.pack $ show route)
    deny <- lookupHeader "X-Deny-Wrapper"
    if deny == Just "yes" then notAuthenticated else handler

authorizeRootR, authorizeAnyR :: RouteAuthorizer (InlineApp a)
authorizeRootR = RouteAuthorizer $ \_ -> do
    record NamedAuthorizing
    deny <- lookupHeader "X-Deny-Named"
    pure $ if deny == Just "yes" then Unauthorized "Named denied" else Authorized
authorizeAnyR = authorizeRootR

authorizeStaticR :: StaticR -> RouteAuthorizer (InlineApp a)
authorizeStaticR StaticLeafR = authorizeRootR

getStaticLeafR :: HandlerFor (InlineApp a) Text
getStaticLeafR = record Handling >> pure "static read"

postStaticLeafR :: HandlerFor (InlineApp a) Html
postStaticLeafR = record Handling >> pure (toHtml ("static write" :: Text))

-- Deliberately no authorizeItemR or authorizeFilesR bindings. The old inline
-- implementation demanded those instead of the enclosing subtree's policy.
authorizeAccountR :: Int -> Text -> AccountR -> RouteAuthorizer (InlineApp a)
authorizeAccountR org account fragment = RouteAuthorizer $ \_ -> do
    record NamedAuthorizing
    pure $ case fragment of
        ItemR 2 | org == 1 && account == "alice" -> Authorized
        FilesR ["one", "two"] | org == 1 && account == "alice" -> Authorized
        _ -> Unauthorized "Subtree denied"

getRootR :: HandlerFor (InlineApp a) Text
getRootR = record Handling >> pure "read root"

postRootR :: HandlerFor (InlineApp a) Html
postRootR = record Handling >> pure (toHtml ("write root" :: Text))

handleAnyR :: HandlerFor (InlineApp a) Text
handleAnyR = record Handling >> pure "any method"

getItemR :: Int -> Text -> Int -> HandlerFor (InlineApp a) Text
getItemR _ _ _ = record Handling >> pure "read item"

postItemR :: Int -> Text -> Int -> HandlerFor (InlineApp a) Html
postItemR _ _ _ = record Handling >> pure (toHtml ("write item" :: Text))

getFilesR :: Int -> Text -> [Text] -> HandlerFor (InlineApp a) Text
getFilesR _ _ _ = record Handling >> pure "files"

eventsShouldBe :: [Event] -> YesodExample (InlineApp ()) ()
eventsShouldBe expected = do
    InlineApp ref <- getTestYesod
    liftIO $ readIORef ref `shouldReturn` expected

-- UrlToDispatch does not infer a phantom foundation argument from the site.
concreteRoute :: Route (InlineApp ()) -> Route (InlineApp ())
concreteRoute = id

spec :: Spec
spec = before (siteToYesodExampleData . InlineApp <$> newIORef []) $ do
    let allowed = [Middleware, LegacyAuthorizing, NamedAuthorizing, Authorizing, Handling, MiddlewareFinished]
        wrapperFailure = [Middleware, LegacyAuthorizing, NamedAuthorizing, Authorizing, MiddlewareFinished, ErrorRendering]
        namedFailure = [Middleware, LegacyAuthorizing, NamedAuthorizing, MiddlewareFinished, ErrorRendering]

    it "wraps a flat Text handler as TypedContent" $ do
        get (concreteRoute RootR)
        statusIs 200
        bodyEquals "read root"
        assertHeader "X-Wrapper-Route" "RootR"
        eventsShouldBe allowed

    it "wraps a flat Html handler with the same concrete wrapper" $ do
        post (concreteRoute RootR)
        statusIs 200
        bodyEquals "write root"
        eventsShouldBe allowed

    it "wraps the flat 405 handler as TypedContent" $ do
        request $ setUrl (concreteRoute RootR) >> setMethod "DELETE"
        statusIs 405
        eventsShouldBe wrapperFailure

    it "denies in the wrapper before a flat 405" $ do
        request $ do
            setUrl (concreteRoute RootR)
            setMethod "DELETE"
            addRequestHeader ("X-Deny-Wrapper", "yes")
        statusIs 401
        eventsShouldBe wrapperFailure

    it "runs named authorization first when both flat checks deny" $ do
        request $ do
            setUrl (concreteRoute RootR)
            addRequestHeader ("X-Deny-Named", "yes")
            addRequestHeader ("X-Deny-Wrapper", "yes")
        statusIs 403
        eventsShouldBe namedFailure

    it "wraps a handler without method restrictions" $ do
        request $ setUrl (concreteRoute AnyR) >> setMethod "DELETE"
        statusIs 200
        bodyEquals "any method"
        eventsShouldBe allowed

    it "uses a subtree binding with no parent captures in inline dispatch" $ do
        get (concreteRoute $ StaticR StaticLeafR)
        statusIs 200
        bodyEquals "static read"
        eventsShouldBe allowed

    it "normalizes the static subtree's Html handler" $ do
        post (concreteRoute $ StaticR StaticLeafR)
        statusIs 200
        bodyEquals "static write"
        eventsShouldBe allowed

    forM_ [Nothing, Just "yes"] $ \denial ->
        it ("checks the static subtree's named policy before a 405: " ++ show denial) $ do
            request $ do
                setUrl (concreteRoute $ StaticR StaticLeafR)
                setMethod "DELETE"
                forM_ denial $ \value -> addRequestHeader ("X-Deny-Named", value)
            statusIs $ maybe 405 (const 403) denial
            eventsShouldBe $ maybe wrapperFailure (const namedFailure) denial

    it "passes the full captured route to the inline wrapper" $ do
        get (concreteRoute $ OrgR 1 (AccountR "alice" (ItemR 2)))
        statusIs 200
        bodyEquals "read item"
        assertHeader "X-Wrapper-Route" "OrgR 1 (AccountR \"alice\" (ItemR 2))"
        eventsShouldBe allowed

    it "normalizes different result types within one inlined leaf" $ do
        post (concreteRoute $ OrgR 1 (AccountR "alice" (ItemR 2)))
        statusIs 200
        bodyEquals "write item"
        eventsShouldBe allowed

    forM_ [(9, "alice", 2), (1, "bob", 2), (1, "alice", 9)] $ \(org, account, item) ->
        it ("passes captures to the inline subtree authorizer: " ++ show (org, account, item)) $ do
            get (concreteRoute $ OrgR org (AccountR account (ItemR item)))
            statusIs 403
            bodyContains "Subtree denied"
            eventsShouldBe namedFailure

    it "checks subtree authorization before an inlined 405" $ do
        request $ setUrl (concreteRoute $ OrgR 1 (AccountR "alice" (ItemR 9))) >> setMethod "DELETE"
        statusIs 403
        eventsShouldBe namedFailure

    it "wraps an authorized inlined 405" $ do
        request $ setUrl (concreteRoute $ OrgR 1 (AccountR "alice" (ItemR 2))) >> setMethod "DELETE"
        statusIs 405
        eventsShouldBe wrapperFailure

    it "passes trailing path pieces to inline subtree authorization" $ do
        get (concreteRoute $ OrgR 1 (AccountR "alice" (FilesR ["one", "two"])))
        statusIs 200
        bodyEquals "files"
        eventsShouldBe allowed

    it "denies invalid trailing path pieces before the inline wrapper" $ do
        get (concreteRoute $ OrgR 1 (AccountR "alice" (FilesR ["private"])))
        statusIs 403
        eventsShouldBe namedFailure
