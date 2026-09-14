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
/deep DeepR LeafSub getLeafSub
|]
