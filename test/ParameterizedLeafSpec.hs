{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module ParameterizedLeafSpec (spec) where

import NestedRouteDispatchSpec.Resources (AuthorizeRoute(..), nestDefaultOpts)
import Test.Hspec
import Test.Hspec.Yesod
import Yesod.Core

-- Neither the root nor OrgR owns endpoints or has an authorization instance.
data ParamApp a = ParamApp

mkYesodOpts (setParameterizedSubroute True nestDefaultOpts) "ParamApp a" [parseRoutes|
/org/#Int OrgR:
    /team/#Int TeamR:
        /item/#Int ItemR GET
|]

instance Yesod (ParamApp a) where
    makeSessionBackend _ = pure Nothing
    messageLoggerSource = mempty

instance AuthorizeRoute (TeamR a) where
    authorizeRoute (org, team) (ItemR item)
        | org == 1 && team == 2 && item == 3 = pure ()
        | otherwise = permissionDenied "wrong capture"

getItemR :: Int -> Int -> Int -> HandlerFor (ParamApp a) String
getItemR _ _ _ = pure "item"

spec :: Spec
spec = before (pure $ siteToYesodExampleData (ParamApp :: ParamApp ())) $ do
    it "dispatches through parameterized parents that own no policy" $ do
        request $ setUrl (OrgR 1 (TeamR 2 (ItemR 3)) :: Route (ParamApp ()))
        statusIs 200
        bodyEquals "item"
    it "preserves all captures through parameterized delegation" $ do
        request $ setUrl (OrgR 0 (TeamR 2 (ItemR 3)) :: Route (ParamApp ()))
        statusIs 403
    it "can request the parameterized leaf without its parent dispatch" $ do
        request $ setUrl (WithParentArgs (1, 2) (ItemR 3 :: TeamR ()))
        statusIs 200
    it "checks the selected leaf before rejecting the method" $ do
        request $ do
            setUrl (WithParentArgs (1, 0) (ItemR 3 :: TeamR ()))
            setMethod "DELETE"
        statusIs 403
