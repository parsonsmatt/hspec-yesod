{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}

module NestedRouteDispatchSpec.Foo.Route where

import NestedRouteDispatchSpec.Resources
import Yesod.Core

mkYesodDataOpts (mkRouteOpts (Just "FooR")) "App" resources
