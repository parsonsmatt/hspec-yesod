# Dispatching on Route Fragments

In [`yesod` PR #1887](https://github.com/yesodweb/yesod/pull/1887), I've introduced the ability to dispatch and render route fragments.
Now, it's time to be able to write tests on them.
First, a bit of code archaelogy:

## How requests are made and dispatched

Currently, we use `request :: RequestBuilder site () -> YesodExample site ()` to perform a request.
This synthesizes an `app :: Application` using the field `yedCreateApplication` on `YesodExampleData` which is a function `site -> Middleware -> IO Application`.
This is currently set in `siteToYesodExampleData`, which is used in `before` hooks to provide a `YesodExampleData site` to our tests.

This design allows us to write, say,

```haskell
spec :: SpecWith (YesodExampleData App)
spec = do
  it "works" $ do
    request $ do buildRequestOnRoute (SomeRouteR (IndexR 10))
```

and we actually don't need `YesodDispatch App` - some *other* module will do `before mkYesodExampleData` and *that* will incur the `YesodDispatch App`.
Still, this means that actually *running* tests requires the `YesodDispatch App`.
We want for *running* the tests to only require the route dispatch for the route themselves, which should drop dependencies.

The building of the site looks like this:

```haskell
siteToYesodExampleData site =
    YesodExampleData
        { yedCreateApplication = \site' middleware -> middleware <$> toWaiAppPlain site'
        , {- snip, lots of memptyish stuff -}
        }
```

The most straightforward thing to do is move the actual application creation back into `request`.
Currently, this is what we do:

```haskell
request
    :: RequestBuilder site ()
    -> YesodExample site ()
request reqBuilder = do
    app <- mkApplication
    site <- MS.gets yedSite
```

We need to use type information from `RequestBuilder` to determine what sort of application we need to create.
Right now, `site` is insufficient - we can only get `YesodDispatch site`.
We need `YesodDispatchNested routeFragment` along with `ParentSite routeFragment ~ site`.
So at least we need to record the type of the request we want.

## `setUrl`

`setUrl` is how we actually choose a route fragment.
And this one is intriguing, because we don't directly set a url.
First, we render it into text, and then we separate that out and stuff it into the `RequestBuilderData` path and gets.

```haskell
setUrl :: (Yesod site, RedirectUrl site url)
       => url
       -> RequestBuilder site ()
setUrl url' = do
    site <- fmap rbdSite YT.SIO.getSIO
    eurl <- Yesod.Core.Unsafe.runFakeHandler
        M.empty
        (const $ error "Test.Hspec.Yesod: No logger available")
        site
        (toTextUrl url')
    url <- either (error . show) return eurl
    let (urlPath, urlQuery) = T.break (== '?') url
    YT.SIO.modifySIO $ \rbd -> rbd
        { rbdPath =
            case DL.filter (/="") $ H.decodePathSegments $ TE.encodeUtf8 urlPath of
                ("http:":_:rest) -> rest
                ("https:":_:rest) -> rest
                x -> x
        , rbdGets = rbdGets rbd ++ H.parseQuery (TE.encodeUtf8 urlQuery)
        }
```

Rather than do all this logic at this point, we will instead want to record the actual `url' :: url` in the `RequestBuilder` - so `RequestBuilder route site a`.
If you use `Text` or `Route site`, then the `request` function will need to do `YesodDispatch site`.
*BUT*, if you do a route fragment datatype, then the request function will need to do `YesodDispatchNested`.

This will probably take the place of `RedirectUrl`.
Copying from the othe repo,

# `setUrl` Call Stack

`setUrl :: (Yesod site, RedirectUrl site url) => url -> RequestBuilder site)`.

So `RedirectUrl` is how we go from an endpoint to an actual url:

```haskell
    eurl <- Yesod.Core.Unsafe.runFakeHandler
        M.empty
        (const $ error "Yesod.Test: No logger available")
        site
        (toTextUrl url')
```

is the exact code.

```haskell
-- | Some value which can be turned into a URL for redirects.
class RedirectUrl master a where
    -- | Converts the value to the URL and a list of query-string parameters.
    toTextUrl :: (MonadHandler m, HandlerSite m ~ master) => a -> m Text

-- the instance for routes:

instance RedirectUrl master (Route master) where
    toTextUrl url = do
        r <- getUrlRender
        return $ r url

-- | Get the URL rendering function.
getUrlRender :: MonadHandler m => m (Route (HandlerSite m) -> Text)
getUrlRender = do
    x <- rheRender <$> askHandlerEnv
    return $ flip x []

-- where that is, anyway
data RunHandlerEnv child site = RunHandlerEnv
    { rheRender   :: !(Route site -> [(Text, Text)] -> Text)
    , rheRoute    :: !(Maybe (Route child))
    -- snip
    }

```

`rheRender` appears to be populated in two places: one in `yesodRunner` and one in `fakeRunHandler`.

```haskell
yesodRunner = ...
      let ra = resolveApproot yreSite req
      let log' = messageLoggerSource yreSite yreLogger
          -- We set up two environments: the first one has a "safe" error handler
          -- which will never throw an exception. The second one uses the
          -- user-provided errorHandler function. If that errorHandler function
          -- errors out, it will use the safeEh below to recover.
          rheSafe = RunHandlerEnv
              { rheRender = yesodRender yreSite ra
              -- snip
              }

runFakeHandler = ...
  let handler' = liftIO . I.writeIORef ret . Right =<< handler
  let yapp = runHandler
         RunHandlerEnv
            { rheRender = yesodRender site $ resolveApproot site fakeWaiRequest
            , rheRoute = Nothing
            -- snip
            }
```

So both call to `yesodRender`.

```haskell
yesodRender :: Yesod y
            => y
            -> ResolvedApproot
            -> Route y
            -> [(Text, Text)] -- ^ url query string
            -> Text
yesodRender y ar url params =
    decodeUtf8With lenientDecode $ BL.toStrict $ toLazyByteString $
    fromMaybe
        (joinPath y ar ps
          $ params ++ params')
        (urlParamRenderOverride y url params)
  where
    (ps, params') = renderRoute url
```

Ah, which uses `renderRoute` !

So, this implies that we need an analog of `yesodRender` here which uses `renderRouteNested` instead of `renderRoute` in order to expose this.
This requires `Yesod y`, but that's not a huge burden.

# Remove `yedCreateApplication`

So `yedCreateApplication` as a *function* to create an `Application` is a difference from `yesod-test`.
`yesod-test` has a full-fledge `Application` on the `YesodExampleData`, so this isn't a huge deal.

I guess we still want to support the case of modifying the middleware.

# Small hitch: getLatestRequest

This *exposed* function returns the `RequestBuilderData` from the most recent request.
This is also stored on the `YesodExampleData` in a `Maybe` field.
This means that some amount of futzing about may be required if I delete other fields - like `rbdPath` as a separate thing.
Additionally, `RequestBuilderData url site` is the *initial* state, so I need a way to say "nothing yet" in the `rbdUrl` - so now it has to be a `Maybe`.
I guess that's not *too* incongruous with the current thing - it does `[]` for the path - but that's actually rather different semantically.
