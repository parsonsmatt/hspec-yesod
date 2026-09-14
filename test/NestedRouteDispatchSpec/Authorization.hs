{-# LANGUAGE TypeFamilies #-}

module NestedRouteDispatchSpec.Authorization where

import Data.Text (Text)
import Yesod.Core hiding (isAuthorized)

-- The application owns both the class and its result type.
class RenderRouteNested route => Authorize route where
    isAuthorized
        :: WithParentArgs route
        -> HandlerFor (ParentSite route) (AuthorizationResult Text)

data AuthorizationResult a
    = Allowed a
    | Denied Text
    | LoginRequired

requireAuthorized :: Authorize route => WithParentArgs route -> HandlerFor (ParentSite route) Text
requireAuthorized route = do
    result <- isAuthorized route
    case result of
        Allowed value -> pure value
        Denied message -> permissionDenied message
        LoginRequired -> notAuthenticated
