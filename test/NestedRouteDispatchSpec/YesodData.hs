{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.YesodData where

import NestedRouteDispatchSpec.Foo.Route (FooR(..))
import NestedRouteDispatchSpec.Resources
import Yesod.Core

mkYesodDataOpts (mkRouteOpts Nothing) "App" resources

instance Yesod App
