{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module MountLeafSpec (spec) where

import Data.Text (Text)
import NestedRouteDispatchSpec.Resources (AuthorizeRoute (..), nestDefaultOpts)
import Test.Hspec
import Test.Hspec.Yesod
import Yesod.Core

data Sub = Sub

mkYesodSubData "Sub" [parseRoutes| /page PageR GET |]

instance Yesod parent => YesodSubDispatch Sub parent where
    yesodSubDispatch = $(mkYesodSubDispatch [parseRoutes| /page PageR GET |])

getPageR :: Yesod parent => SubHandlerFor Sub parent Text
getPageR = maybe "missing policy" id <$> lookupSession "mount-policy"

data App = App

mkYesodOpts nestDefaultOpts "App" [parseRoutes|
/group/#Int GroupR:
    /mount/#Int MountR Sub getSub
|]

instance Yesod App where
    makeSessionBackend _ = pure Nothing
    messageLoggerSource = mempty

getSub :: App -> Int -> Int -> Sub
getSub _ _ _ = Sub

-- GroupR owns the mount, so no AuthorizeRoute (Route App) instance is needed.
instance AuthorizeRoute GroupR where
    authorizeRoute parent (LeafMountR capture selected)
        | parent /= 1 || capture /= 2 = permissionDenied "mount denied"
        | otherwise = case selected of
            Just PageR -> setSession "mount-policy" "authorized"
            Nothing -> setSession "mount-policy" "checked miss"

spec :: Spec
spec = describe "mount leaf authorization with ordinary requests" $
    before (pure $ siteToYesodExampleData App) $ do
        it "supplies a mount policy through focused dispatch" $ do
            request $ setUrlNested 1 (MountR 2 PageR)
            statusIs 200
            bodyEquals "authorized"
        it "denies parent and mount captures before the handler" $ do
            request $ setUrlNested 0 (MountR 2 PageR)
            statusIs 403
            request $ setUrlNested 1 (MountR 0 PageR)
            statusIs 403
        it "checks mount policy before a matched-path 405" $ do
            request $ do
                setUrlNested 0 (MountR 2 PageR)
                setMethod "POST"
            statusIs 403
            request $ do
                setUrlNested 1 (MountR 2 PageR)
                setMethod "POST"
            statusIs 405
        it "runs the mount policy on misses and lets it preserve a 404" $ do
            request $ setUrl ("/group/1/mount/2/missing" :: Text)
            statusIs 404
            request $ setUrl ("/group/0/mount/2/missing" :: Text)
            statusIs 403
