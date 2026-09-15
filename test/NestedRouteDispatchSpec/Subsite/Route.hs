{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module NestedRouteDispatchSpec.Subsite.Route where

import Yesod.Core

data LeafSub = LeafSub

mkYesodSubData "LeafSub" [parseRoutes|
/ LeafR GET
|]

data AuthSub = AuthSub

mkYesodSubData "AuthSub" [parseRoutes|
/page PageR GET
/writable WritableR GET DELETE
/deep DeepR LeafSub getLeafSub
/wai WaiR WaiSubsiteWithAuth getAuthWai
/group GroupR:
    /wai GroupWaiR WaiSubsiteWithAuth getAuthWai
|]
