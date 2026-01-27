{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.YesodDispatch where

import NestedRouteDispatchSpec.Foo.Handler () -- needed for the yesod dispatch instance in scope
import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.YesodData
import Yesod.Core
import Data.Text (Text)

mkYesodDispatchOpts (mkRouteOpts Nothing) "App" resources

getHomeR :: HandlerFor App Text
getHomeR = pure "HomeR"
