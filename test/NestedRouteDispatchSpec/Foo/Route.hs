{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}

module NestedRouteDispatchSpec.Foo.Route where

import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.Subsite.Route
import Yesod.Core

mkYesodDataOpts (nestDefaultOptsFor "FooR") "App" resources
