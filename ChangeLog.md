# ChangeLog for hspec-yesod

## 0.3.0.0

- [#7](https://github.com/parsonsmatt/hspec-yesod/pull/7)
    - Require `yesod-core >= 1.7.0.0`.
    - You can now test a subset of routes without depending on the full
      `YesodDispatch`, which can dramatically cut test compile times. `setUrl`
      and the new `setUrlNested` accept route fragments via `RequestBuilderFor`,
      which carries the route-fragment type.
    - **Breaking:** `RequestBuilderData` gained a `url` type parameter
      (`RequestBuilderData url site`); `getLatestRequest`/`requireLatestRequest`
      return `RequestBuilderData () site`.
    - **Breaking:** `get`, `post`, `postBody`, `performMethod`, and `request`
      now require `UrlToDispatch url site` (from `yesod-core`) instead of
      `RedirectUrl site url`.
    - **Breaking:** a request must now set a URL explicitly; previously an unset
      URL defaulted to `/`, now `request` fails with an error.
    - **Breaking:** removed `TestApp`, `mkTestApp`, and the
      `yedCreateApplication` field of `YesodExampleData`.
    - Added `getRunnerEnv`, which builds a `YesodRunnerEnv` for the current
      `yedSite` via `mkYesodRunnerEnv`. It is built fresh per request so it
      always reflects per-request foundation changes.
    - The `YesodDispatch site` constraint is no longer required by `yesodSpec`,
      `yesodSpecWithSiteGenerator`, `yesodSpecWithSiteGeneratorAndArgument`, or
      `siteToYesodExampleData`.
    - WAI middlewares are now applied in the request builder.
    - Add a `CallStack` to `requireLatestRequest`.
    - Assertion failures now use `withFrozenCallStack`, and the debug formatters
      decode request bytes leniently, so a failure message can no longer be
      masked by a `UnicodeException`.

## 0.2.1.1

- [#8](https://github.com/parsonsmatt/hspec-yesod/pull/8)
  - When `bodyContains` or `bodyNotContains` fail and the content is UTF-8, a partial body is printed to aid debugging. This matches the behavior of `statusIs`.

## 0.2.1.0

- [#6](https://github.com/parsonsmatt/hspec-yesod/pull/6)
  - The function `testModifySite` is deprecated and replaced with `testModifyMiddleware`, `testModifyFoundation`, and `testModifyFoundationAndMiddleware`.

## 0.2.0.1

- Fixes unused import warnings in `Test.Hspec.Yesod.Internal`
- `statusIs` now also prints the first 1024 characters of the request body, in certain circumstances.
- `statusIs` output broken across multiple lines

## 0.2.0

- `statusIs` now prints the request method, path, and query string on failing requests
- Tracks the latest request via `yedRequest` on `YesodExampleData`
- Adds `getLatestRequest` and `requireLatestRequest` as helpful accessors for it 
- Adds `formatRequestBuilderDataForDebugging` to format the request for use in error messages
- Adds a new `.Internal` module to access fields of `RequestBuilderData`

Together these changes are intended to allow for better error messages, like the one made to `statusIs`.

## 0.1.0

- Initial release and fork from `yesod-test`.
