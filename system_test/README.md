# System tests

This project runs tests against the aircloak system. The components, such as air and cloak, are started as release containers. Therefore, the tests here are treating the system as a black box, and interact with it using supported API calls.

## Air-setup

The Air is setup automatically using a predefined privacy policy and user accounts.
Two sets of user accounts are created:

- `admin@aircloak.com` and `user@aircloak.com` are created by the
  [Seeder](https://github.com/Aircloak/aircloak/blob/master/air/lib/air/repo/seeder.ex)-module
- `admin` and `user` are created using the automatic creation of users during boot functionality

All users share the password `password1234`.

## Running

To start the system locally invoke `make dev-container`. Then you can invoke `mix test` to run the tests.

Keep in mind that supporting cloak and air are running as release containers, so you can't change your local code and have the changes applied automatically. However, you can freely edit the test code while in the container, because source files of tests are mounted in the dev container.
