{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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

-- No root or unrelated authorization instances are imported here.
instance AuthorizeRoute FooR where
    authorizeRoute parent leaf = case leaf of
        LeafFooIndexR -> check parent
        LeafFooEditR -> permissionDenied "edit denied"
        LeafFooShowR child -> check parent >> check child
      where
        check value = if value > 0 then pure () else permissionDenied "capture denied"

mkYesodDispatchOpts (nestDefaultOptsFor "FooR") "App" resources

getFooIndexR :: Int -> HandlerFor App Text
getFooIndexR i = pure $ "getFooIndexR: " <> Text.pack (show i)

getFooEditR :: Int -> HandlerFor App Text
getFooEditR i = pure $ "getFooEditR: " <> Text.pack (show i)

getFooShowR :: Int -> Int -> HandlerFor App Text
getFooShowR i j = pure $ "getFooShowR: " <> Text.pack (show (i, j))
