{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.Foo.Handler where

import NestedRouteDispatchSpec.Foo.Route
import NestedRouteDispatchSpec.Resources
import qualified Data.Text as Text
import Data.Text (Text)
import Yesod.Core

mkYesodDispatchOpts (mkRouteOpts (Just "FooR")) "App" resources

getFooIndexR :: Int -> HandlerFor App Text
getFooIndexR i = pure $ "getFooIndexR: " <> Text.pack (show i)

getFooEditR :: Int -> HandlerFor App Text
getFooEditR i = pure $ "getFooEditR: " <> Text.pack (show i)

getFooShowR :: Int -> Int -> HandlerFor App Text
getFooShowR i j = pure $ "getFooEditR: " <> Text.pack (show (i, j))
