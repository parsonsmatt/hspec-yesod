# Authorization regression coverage

These fixtures exercise the public APIs added by
[Yesod PR #1931](https://github.com/yesodweb/yesod/pull/1931) through the existing
hspec-yesod test suite. `cabal.project` and `stack.yaml` pin the Yesod revision.
Run the complete suite with either:

```sh
cabal test all --test-show-details=direct
stack test
```

The suite contains 288 examples: 223 authorization/dispatch examples and the
65 other existing examples.

`Foo.HandlerSpec`, `Account.HandlerSpec`, and `Static.HandlerSpec` dispatch directly to their
`YesodDispatchNested` instances using `get`, `setUrl`, and `setUrlNested`.
None imports the whole-site dispatcher or a sibling's authorizers, even
transitively. The foundation in `YesodData` imports only route data and records
legacy authorization calls without depending on fragment authorizers.
`WholeSiteSpec` separately checks the assembled application's dispatch.

`ParameterizedHookSpec` combines `setParameterizedSubroute True` with the shared
class-based hook. Whole-site and direct nested dispatch both check parent and
leaf captures, unit parent arguments, Text and Html handlers, and authorization
before 405s. Event traces verify that denied requests do not run handlers.

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

## Third review pass

This is the disposition of all 16 numbered findings in the review at Yesod
`943c54b2`. Items #1 and #6 are documented scope decisions; the remaining
items have code, test, or documentation fixes. The lower-priority notes that
remain outside the authorization work are tracked separately for followup.

| Finding | Regression coverage or scope decision |
| --- | --- |
| #1: Transitive raw WAI mounts bypass the parent runner | The runner contract now explicitly applies at every level. Raw `WaiSubsite` and `EmbeddedStatic` remain unsupported under named mounts, including transitively; TH cannot inspect arbitrary subsite bodies. Flat and fragment tests cover the supported replacement, `WaiSubsiteWithAuth`, through both `subTopDispatch` and a separately generated `YesodSubDispatchNested` instance, with allow/deny decisions and exact event traces. |
| #2 and #3: Unresolved mount types and type families escape validation | `AuthorizationTHSpec` hides the unqualified `WaiSubsite` type at the splice and tests qualified controls, aliases, closed/open/nullary families, and even a family reducing to the supported WAI type. Named policies reject unresolved or family types; defaults remain accepted. The existing real `EmbeddedStatic` cases pin its canonical module name. |
| #4: Delegated policies | Named parent splices now warn at existing fragment boundaries. The root and Account dispatch splices still own distinct policies; whole-site tests check both Account's allow and denial without a resource-named Account authorizer. The binding guarantee is explicitly limited to leaves emitted by the owning splice. |
| #5: Mount hits and misses differ under an override | Flat and fragment tests pair a DELETE hit classified as a read with a DELETE miss using the default write policy. Login-required misses check HTML 303 and JSON 401, preserve `_ULT`, and skip legacy authorization. |
| #6: Ancestor policies | Nearest-parent subtree selection remains intentional and is documented. The inline and nested fixtures compile with only the selected subtree bindings; any ancestor access checks must be included in those policies. |
| #7: Public Haddock links | References use public pages and the explicit `RouteAuthSpec` type anchor. Rendered links on `Yesod.Core`, `Yesod.Core.Types`, and `Yesod.Core.Dispatch` were checked against the generated anchors. |
| #8: Generic mount validation | `mkDispatchClause` rejects named mounts whose type it cannot inspect. The yesod-core `untypedMountFailures` test retains the default-option control and checks both named policies. |
| #9: Inline PerResource parent argument spine | `InlineResourceSpec` uses two ancestor captures, a captured leaf, and multipieces on a parameterized foundation. Each capture is independently varied; reads, writes, and 405 denial are checked. |
| #10: Mount-wrapper remedy | The error, Haddock, and guide explain that enabling a named policy demands bindings for all leaves emitted by the splice. They describe moving mounts into focused route blocks when other leaves should remain wrapper-only. |
| #11: Authorization ordering docs | `RouteAuthSpec` defines the order: default middleware's legacy check, named check, wrapper, handler. Other authorization docs link to it; exact event traces pin the order and denial behavior. |
| #12: Public subsite entry points | Their Haddocks distinguish rejected site authorization options from skipped options and callbacks in data-only splices, with links to `RouteAuthSpec`. |
| #13: Deriving subsite options | `subsiteRouteOpts` and rejection share the internal `SiteAuthorization` definition. The projection is tested through both public subsite dispatch generators; unprojected site options continue to fail explicitly. |
| #14: Duplicated default method dispatch | Default and opt-in dispatch now share method selection and `TypedContent` normalization. Existing legacy routing tests and authorization tests exercise that same code path. |
| #15: Duplicated subtree state | `NestedPhase` carries the nearest subtree name, removing the independent `envSubtree` field. Existing inline, subtree-policy, and fallthrough fixtures cover the resulting dispatch. |
| #16: Duplicated mount validation and positional wrapper | `mdsHandlerWrapper` travels beside `mdsRouteAuth`, and both mount-generation paths use `validateMount`. The mount matrix exercises flat, inline, generated nested, and focused dispatch. |

Documentation fixes also clarify ordering (legacy, named, wrapper, handler),
the scope of bindings demanded by a named policy, and the public subsite entry
points. Internal generator hardening and the rendered public Haddock links are
checked in yesod-core.

The related notes below the review's numbered list are also handled: the
subtree fallback reuses per-resource authorizer construction, subsite
rejections explain both `RouteOpts` and `MkDispatchSettings`, the normalized
runner contract is documented, and the changelog links the Yesod PR.

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

For the third-pass validation checks, restore only `validateMountType` from
Yesod `943c54b2` in a temporary source export of the current library. Keep the
new public option helper so this tests the validator independently of API
availability. At the third-pass checkpoint, the 274-example suite compiled and
failed 24 rejection examples (unresolved names and type families); the other
250 examples passed. Restoring the current validator made all 274 pass.
