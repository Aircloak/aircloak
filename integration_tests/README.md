# Integration tests

This project tests integration between air, cloak, and central. The project can only be used in test environment:

```
$ MIX_ENV=test mix deps.get
$ mix test
```

## Acceptance tests

The project also includes acceptance tests. These tests are executed via Chrome headless browser.

The acceptance tests are by default disabled on the developer machine. To run them locally, you need to perform the following steps:

1. `docker run --rm -it -p 4444:4444 --shm-size=2g selenium/standalone-chrome:3.13.0`
2. `export AIR_IP=IP_ADDRESS`, where the `IP_ADDRESS` is the address of the developer machine in your local network (e.g. `192.168.1.2`)
3. `mix test --include acceptance` or `mix test --only acceptance`

Instead of docker container, you can use [chromedriver](http://chromedriver.chromium.org/). In this case, you can perform the following steps:

1. Install chromedriver on your machine
2. `export AIR_IP=localhost`
3. Create `config/test.local.exs`, and copy the configuration from `config/test.local.exs.example`.

The benefit of the chromedriver is that it's possible to run tests inside the browser window, which can help with visual debugging. The downside, at least on a macOS, is that the driver is buggy, so some occasional crashes might happen, especially if you're running too many tests at once.
