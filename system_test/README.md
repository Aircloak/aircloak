# System tests

This project runs tests against the aircloak system. The components, such as air and cloak, are started as release containers. Therefore, the tests here are treating the system as a black box, and interact with it using supported API calls.

## Running

To start the system locally invoke `make dev-container`. Then you can invoke `mix test` to run the tests.

Keep in mind that supporting cloak and air are running as release containers, so you can't change your local code and have the changes applied automatically. However, you can freely edit the test code while in the container, because source files of tests are mounted in the dev container.
