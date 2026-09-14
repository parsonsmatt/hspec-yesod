module NestedRouteDispatchSpec.Assertions where

import Data.IORef (readIORef)
import NestedRouteDispatchSpec.Resources
import Test.Hspec (shouldReturn)
import Test.Hspec.Yesod
import Yesod.Core (liftIO)

eventsShouldBe :: [Event] -> YesodExample App ()
eventsShouldBe expected = do
    App events <- getTestYesod
    liftIO $ readIORef events `shouldReturn` expected

authorizedEvents :: [Event]
authorizedEvents = [Middleware, LegacyAuthorizing, NamedAuthorizing, Authorizing, Handling, MiddlewareFinished]

wrapperDeniedEvents :: [Event]
wrapperDeniedEvents = [Middleware, LegacyAuthorizing, NamedAuthorizing, Authorizing, MiddlewareFinished, ErrorRendering]

namedDeniedEvents :: [Event]
namedDeniedEvents = [Middleware, LegacyAuthorizing, NamedAuthorizing, MiddlewareFinished, ErrorRendering]
