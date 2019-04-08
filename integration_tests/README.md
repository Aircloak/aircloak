# Integration tests

This project tests integration between air, cloak, and central. The project can only be used in test environment:

```
$ MIX_ENV=test mix deps.get
$ mix test
```

## Acceptance tests

The project also includes acceptance tests. These tests are executed via Chrome headless browser.

The acceptance tests are by default disabled on the developer machine. To run them locally, you need to perform the following steps:

1. `docker run --rm -it -p 4444:4444 --shm-size=2g selenium/standalone-chrome:latest`
2. `export AIR_IP=IP_ADDRESS`, where the `IP_ADDRESS` is the address of the developer machine in your local network (e.g. `192.168.1.2`)
3. `mix test --include acceptance` or `mix test --only acceptance`
