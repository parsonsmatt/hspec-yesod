# Authorization regression coverage

These fixtures exercise the public APIs added by
[Yesod PR #1931](https://github.com/yesodweb/yesod/pull/1931) through the existing
hspec-yesod test suite. `cabal.project` and `stack.yaml` pin the Yesod revision.
Run the complete suite with either:

```sh
cabal test all --test-show-details=direct
stack test
```

`Foo.HandlerSpec`, `Account.HandlerSpec`, and `Static.HandlerSpec` dispatch directly to their
`YesodDispatchNested` instances using `get`, `setUrl`, and `setUrlNested`.
None imports the whole-site dispatcher or a sibling's authorizers, even
transitively. The foundation in `YesodData` imports only route data and records
legacy authorization calls without depending on fragment authorizers.
`WholeSiteSpec` separately checks the assembled application's dispatch.

## First review pass

| Finding | Regression coverage |
| --- | --- |
| #1: Subsite splices silently ignored authorization options | `AuthorizationTHSpec` uses TH `recover` to require rejection of both named modes and wrappers through `mkYesodSubDispatchInstanceOpts` and `mkNestedSubDispatchInstance`. Default-option controls must still succeed. |
| #2: Subsite mounts skipped named authorization | `Foo.HandlerSpec` and `WholeSiteSpec` check flat and nested mounts, parent and mount captures, a second subsite, allow/deny decisions, and authorization before subsite 404s and 405s. |
| #4: Inline dispatch used leaf bindings under `RouteAuthSubtree` | `InlineSpec` supplies only the enclosing subtree binding for nested leaves. Compilation fails if leaf bindings are required again. Requests check both parent captures, leaf captures, multipieces, and 405s. |
| #5: Wrappers received inconsistent handler result types | `Authorization.withAuthorization` and `InlineSpec.typedWrapper` require `HandlerFor site TypedContent`. Text and Html handlers and 405 arms must compile and execute through the same wrapper in flat, inline, and nested dispatch. The flat fixture also checks a handler without method restrictions. |
| #8 and #10: Middleware scope and authorization ordering | Exact event traces check middleware entry, legacy authorization, named authorization, wrapper, handler, middleware `finally`, and error rendering. Conflicting denials establish which check wins and that later checks and handlers do not run. |
| #11: Write classification without a matched route | Unmatched subsite paths reach mount authorization without a current route. Both dispatch paths check GET, HEAD, OPTIONS, TRACE, POST, PUT, PATCH, and DELETE against the default method policy. A header that changes the site's policy must affect matched routes, while missing routes use the default. |
| #12: Repeated TH wrapper callbacks | `AuthorizationTHSpec` counts callbacks through public splices: once per leaf regardless of method count or 405 arms, and zero in site/subsite data-only generation. |
| #13: Single parent argument shape | `Foo.HandlerSpec` checks single captures; `Account.HandlerSpec` checks tuple captures independently; `InlineSpec` requires `WithParentArgs ()` with the full route. Each captured value is varied to establish that authorization receives it. |
| #14: Clearing a shared wrapper | `WholeSiteSpec` exercises `unsetRouteHandlerWrapper` without any `Authorize (Route App)` instance. Compilation requires the wrapper to be cleared; requests require named authorization and delegated fragment wrappers to remain active. |

Additional cases check named authentication's HTML redirect and JSON 401,
wrapper 401/403 responses, handler errors without reauthorization, and changed
captures or permissions across successive requests. Response status, body or
headers, and event traces establish both the result and whether the handler ran.

## Second review pass

| Finding | Regression coverage |
| --- | --- |
| #1: Named mount checks on subsites that bypass the runner | `AuthorizationTHSpec` checks the real `WaiSubsite` and `EmbeddedStatic` types, qualified names, and ordinary, applied, and repeated aliases. Default options remain accepted; named checks are rejected in flat, generated nested, focused, and inline dispatch. `WaiSubsiteWithAuth`, its repeated alias, and an unrelated local type named `EmbeddedStatic` are accepted controls. |
| #2: Handler wrappers did not cover mounts | The same matrix requires wrapper-only mounts to fail and named policies with wrappers to accept subsites that honor the runner. Runtime mount cases continue to check the named policy and its 404 behavior. |
| #3 and #4: Authorization scope was unclear | The inline fixture compiles without an ancestor `authorizeOrgR`; only its nearest subtree policy is required. `WholeSiteSpec` dispatches into Account's wrapper-only instance from a root using `RouteAuthPerResource`, proving that the child splice owns its policy. No `authorizeAccountItemR` binding exists. |
| #6: Named authentication and sessions | `Foo.HandlerSpec` checks the saved `_ULT` destination after an HTML redirect, preserves an existing destination for JSON 401s, and checks both response formats when there is no login route. |
| #7: Missing-route write-policy fallback | The first-pass method matrix already reaches this branch through flat and nested subsite 404s. The guide now states the fallback explicitly. |
| #9: Data-only splices ran dispatch codegen | Shared options include a callback that throws and named policies on raw WAI and embedded mounts. Both public data-only splices must succeed without executing the callback or rejecting the dispatch configuration. |
| #14: Parents without captures | `Static.HandlerSpec` dispatches bare fragments and `WithParentArgs ()`, exercising named resource checks, a concrete class wrapper, heterogeneous handler results, and 405s. `InlineSpec` checks the zero-capture subtree binding as well. |
| #15: Clearing a wrapper only tested a record update | The existing `WholeSiteSpec` cases execute the cleared root dispatcher, checking allowed and denied named policies and preservation of delegated wrappers. |

The `yesod-static` dependency is test-only. The Cabal project constrains its
crypton dependencies to versions compatible with its current use of `memory`;
the separate package migration is tracked outside this authorization change.

## Tests kept in yesod-core

The custom `mdsRunHandler` regression (first-pass #9) uses an internal TH setting that is
not exposed by the installed package, so its dedicated test stays in
yesod-core, alongside a counter for named runner generation (second-pass #8).
The rendered Haddock lambda (first-pass #15) is checked there as documentation.
The unrelated form findings are outside these authorization fixtures.

## Checking that the regressions are detected

As a negative control, temporarily removing the named authorization prefix from
both generated subsite parent runners must let this suite compile, then fail
the mount tests. In particular, denied mounted requests would incorrectly
return 200 and authorized requests would omit `NamedAuthorizing` from their
event traces. Use a temporary source export and a separate Cabal project/build
directory for this check; the normal dependency pins should continue to use
the fixed Yesod revision.

The second-pass mount-validation matrix also compiles against the previous
Yesod revision. Its rejection expectations must fail there, establishing that
the new tests detect the formerly accepted configurations.
