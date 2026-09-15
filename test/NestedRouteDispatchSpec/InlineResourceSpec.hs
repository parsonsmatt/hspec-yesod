{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module NestedRouteDispatchSpec.InlineResourceSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Test.Hspec (Spec, before, it)
import Test.Hspec.Yesod
import Yesod.Core

-- No parameterized subroutes: this exercises the inline PerResource spine,
-- including ancestor captures before the leaf and multipiece captures.
data App a = App

mkYesodOpts (setRouteAuthorization RouteAuthPerResource defaultOpts) "App a" [parseRoutes|
/org/#Int OrgR:
    /account/#Text AccountR:
        /item/#Int ItemR GET POST
        /files/*Texts FilesR GET
|]

instance Yesod (App a) where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing

authorizeItemR :: Int -> Text -> Int -> RouteAuthorizer (App a)
authorizeItemR org account item = RouteAuthorizer $ \isWrite ->
    pure $ if (org, account, item) == (1, "alice", 2) && not isWrite
        then Authorized else Unauthorized "item denied"

authorizeFilesR :: Int -> Text -> [Text] -> RouteAuthorizer (App a)
authorizeFilesR org account pieces = RouteAuthorizer $ \_ ->
    pure $ if (org, account, pieces) == (1, "alice", ["one", "two"])
        then Authorized else Unauthorized "files denied"

getItemR :: Int -> Text -> Int -> HandlerFor (App a) Text
getItemR _ _ _ = pure "item"

postItemR :: Int -> Text -> Int -> HandlerFor (App a) Html
postItemR _ _ _ = pure (toHtml ("posted" :: Text))

getFilesR :: Int -> Text -> [Text] -> HandlerFor (App a) Text
getFilesR _ _ _ = pure "files"

spec :: Spec
spec = before (pure $ siteToYesodExampleData (App :: App ())) $ do
    it "passes both ancestor captures before the leaf capture" $ do
        get ("/org/1/account/alice/item/2" :: String)
        statusIs 200
        bodyEquals "item"
    forM_ ["/org/9/account/alice/item/2", "/org/1/account/bob/item/2", "/org/1/account/alice/item/9"] $ \path ->
        it ("denies changed parent or leaf captures: " ++ path) $ do
            get path
            statusIs 403
            bodyContains "item denied"
    forM_ ["POST", "DELETE"] $ \method ->
        it ("authorizes the selected handler or 405: " ++ show method) $ do
            request $ setUrl ("/org/1/account/alice/item/2" :: String) >> setMethod method
            statusIs 403
            bodyContains "item denied"
    it "passes ancestor captures before trailing multipieces" $ do
        get ("/org/1/account/alice/files/one/two" :: String)
        statusIs 200
        bodyEquals "files"
    forM_ ["/org/9/account/alice/files/one/two", "/org/1/account/bob/files/one/two", "/org/1/account/alice/files/private"] $ \path ->
        it ("denies changed captures on a multipiece route: " ++ path) $ do
            get path
            statusIs 403
            bodyContains "files denied"
