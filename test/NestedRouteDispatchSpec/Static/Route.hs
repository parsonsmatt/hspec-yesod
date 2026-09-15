{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module NestedRouteDispatchSpec.Static.Route where

import NestedRouteDispatchSpec.Resources
import Yesod.Core

mkYesodDataOpts (nestDefaultOptsFor "StaticR") "App" resources
