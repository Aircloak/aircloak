# Integration tests

This is project bundles air and cloak, and tests end-to-end functionality. The project can only be used in test environment:

```
$ MIX_ENV=test mix deps.get
$ mix test
```

## Acceptance tests

The project also includes acceptance tests. These tests are executed via phantomjs headless browser, which can be downloaded [here](http://phantomjs.org/download.html). After downloading, make sure that `phantomjs` is in the executable path.

The tests are by default disabled on the developer machine. You can include them by passing `--include acceptance` or `--only acceptance` to `mix test`.
