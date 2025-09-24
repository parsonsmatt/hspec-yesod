# ChangeLog for hspec-yesod

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
