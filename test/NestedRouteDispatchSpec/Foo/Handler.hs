{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NestedRouteDispatchSpec.Foo.Handler where

import NestedRouteDispatchSpec.Foo.Route
import NestedRouteDispatchSpec.Resources
import NestedRouteDispatchSpec.Authorization
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Network.Wai as W
import Yesod.Core hiding (isAuthorized)

instance Authorize FooR where
    isAuthorized (WithParentArgs parent route) = do
        recordEvent Authorizing
        method <- W.requestMethod <$> waiRequest
        canWrite <- lookupHeader "X-Allow-Write"
        pure $ case route of
            _ | parent /= 1 -> Denied "Wrong parent"
            FooLoginRequiredR -> LoginRequired
            FooShowR item | item /= 2 -> Denied "Wrong item"
            FooFilesR pieces | pieces /= ["one", "two"] -> Denied "Wrong files"
            _ | method `notElem` ["GET", "HEAD", "OPTIONS", "TRACE"]
                  && canWrite /= Just "yes" -> Denied "Writes require permission"
            _ -> Allowed "permission granted"

mkYesodDispatchOpts
    (setRouteHandlerWrapper
        (\handler route -> [| requireAuthorized $route >> $handler |])
        (nestDefaultOptsFor "FooR"))
    "App"
    resources

getFooIndexR :: Int -> HandlerFor App Text
getFooIndexR i = recordEvent Handling >> pure ("getFooIndexR: " <> Text.pack (show i))

postFooIndexR :: Int -> HandlerFor App Text
postFooIndexR i = recordEvent Handling >> pure ("postFooIndexR: " <> Text.pack (show i))

getFooEditR :: Int -> HandlerFor App Text
getFooEditR i = recordEvent Handling >> pure ("getFooEditR: " <> Text.pack (show i))

getFooShowR :: Int -> Int -> HandlerFor App Text
getFooShowR i j = recordEvent Handling >> pure ("getFooShowR: " <> Text.pack (show (i, j)))

getFooFilesR :: Int -> [Text] -> HandlerFor App Text
getFooFilesR _ pieces = recordEvent Handling >> pure (Text.intercalate "/" pieces)

getFooLoginRequiredR :: Int -> HandlerFor App Text
getFooLoginRequiredR _ = recordEvent Handling >> pure "private"
