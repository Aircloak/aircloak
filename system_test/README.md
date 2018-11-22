# System tests

This project runs tests against the aircloak system. The components, such as air and cloak, are started as release containers. Therefore, the tests here are treating the system as a black box, and interact with it using supported API calls.

## Air-setup

The Air is setup automatically using a predefined license, privacy policy and user accounts.
Users `admin@aircloak.com` and `user@aircloak.com` are created. Both have password `password1234`, 
and both have access to all data sources.

## Running

To start the system locally invoke `make dev-container`. Then you can invoke `mix test` to run the tests.

Keep in mind that supporting cloak and air are running as release containers, so you can't change your local code and have the changes applied automatically. However, you can freely edit the test code while in the container, because source files of tests are mounted in the dev container.
