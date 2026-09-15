{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- No Foo authorizer or whole-site dispatcher is imported here.
module NestedRouteDispatchSpec.Account.Handler where

import Data.Text (Text)
import NestedRouteDispatchSpec.Account.Route
import NestedRouteDispatchSpec.Authorization
import NestedRouteDispatchSpec.Resources
import Yesod.Core hiding (isAuthorized)

instance Authorize AccountR where
    isAuthorized (WithParentArgs (org, account) (AccountItemR item)) = do
        recordEvent Authorizing
        pure $ if org == 1 && account == "alice" && item == 2
            then Allowed "account" else Denied "Account denied"

mkYesodDispatchOpts (routeAuthOpts $ nestDefaultOptsFor "AccountR") "App" resources

getAccountItemR :: Int -> Text -> Int -> HandlerFor App Text
getAccountItemR _ _ _ = recordEvent Handling >> pure "account item"
