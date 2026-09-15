{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module NestedRouteDispatchSpec.ParameterizedHookSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types (statusCode)
import qualified Network.Wai as W
import qualified Network.Wai.Test as WT
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldReturn)
import Yesod.Core hiding (isAuthorized)

import NestedRouteDispatchSpec.Authorization
    (Authorize(..), AuthorizationResult(..), routeAuthOpts)

data App a = App (IORef [String])

-- Opt into parameterized fragments rather than inline compatibility dispatch.
-- The shared hook must resolve Authorize (OrgR a) and Authorize (StaticR a)
-- without requiring an Authorize (Route (App a)) instance.
mkYesodOpts (routeAuthOpts $ setParameterizedSubroute True defaultOpts) "App a" [parseRoutes|
/org/#Int OrgR:
    /item/#Int ItemR GET POST
/static StaticR:
    / StaticLeafR GET
|]

instance Yesod (App a) where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing

record :: String -> HandlerFor (App a) ()
record event = do
    App events <- getYesod
    liftIO $ modifyIORef' events (++ [event])

instance Authorize (OrgR a) where
    isAuthorized (WithParentArgs org (ItemR item)) = do
        record "authorize item"
        pure $ if org == 1 && item == 2
            then Allowed "item"
            else Denied "item denied"

instance Authorize (StaticR a) where
    isAuthorized (WithParentArgs () StaticLeafR) = do
        record "authorize static"
        pure $ Allowed "static"

getItemR :: Int -> Int -> HandlerFor (App a) Text
getItemR _ _ = record "handler" >> pure "item"

postItemR :: Int -> Int -> HandlerFor (App a) Html
postItemR _ _ = record "handler" >> pure (toHtml ("posted" :: Text))

getStaticLeafR :: HandlerFor (App a) Text
getStaticLeafR = record "handler" >> pure "static"

checkRequest
    :: (App () -> IO Application)
    -> ByteString -> [Text] -> Int -> Maybe LBS.ByteString -> [String]
    -> Expectation
checkRequest makeApp method path expectedStatus expectedBody expectedEvents = do
    events <- newIORef []
    app <- makeApp (App events)
    response <- WT.runSession (WT.request WT.defaultRequest
        { W.requestMethod = method, W.pathInfo = path }) app
    statusCode (WT.simpleStatus response) `shouldBe` expectedStatus
    forM_ expectedBody $ \body -> WT.simpleBody response `shouldBe` body
    readIORef events `shouldReturn` expectedEvents

spec :: Spec
spec = forM_
    [ ("whole-site dispatch", \_ -> toWaiAppPlain, toWaiAppPlain)
    , ("direct nested dispatch",
        toWaiAppPlainNested (Proxy :: Proxy (OrgR ())),
        toWaiAppPlainNested (Proxy :: Proxy (StaticR ())) ())
    ] $ \(label, orgApp, staticApp) -> describe label $ do
        let checkItem method org item = checkRequest (orgApp org) method
                ["org", Text.pack (show org), "item", Text.pack (show item)]
            authorizedEvents = ["authorize item", "handler"]
            checkedEvents = ["authorize item"]
        it "authorizes a parameterized fragment before its Text handler" $
            checkItem "GET" 1 2 200 (Just "item") authorizedEvents
        it "uses the same typed hook for an Html handler" $
            checkItem "POST" 1 2 200 (Just "posted") authorizedEvents
        it "denies a changed parent capture without running the handler" $
            checkItem "GET" 3 2 403 Nothing checkedEvents
        it "denies a changed leaf capture without running the handler" $
            checkItem "GET" 1 3 403 Nothing checkedEvents
        it "reports 405 after successful authorization" $
            checkItem "DELETE" 1 2 405 Nothing checkedEvents
        it "denies an unauthorized method mismatch before reporting 405" $
            checkItem "DELETE" 1 3 403 Nothing checkedEvents
        it "authorizes a parameterized fragment with unit parent arguments" $
            checkRequest staticApp "GET" ["static"] 200 (Just "static")
                ["authorize static", "handler"]
