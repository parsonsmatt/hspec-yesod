{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module NestedRouteDispatchSpec.Account.Route where

import NestedRouteDispatchSpec.Resources
import Yesod.Core
import Data.Text (Text)

mkYesodDataOpts (nestDefaultOptsFor "AccountR") "App" resources
