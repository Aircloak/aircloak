This document serves to describe breaking changes and provide upgrade hints when major changes are introduced. When you're creating a pull with some major changes, please add brief upgrade instructions here.

## Running air (insights) through the router

- New pseudo local site has been introduced: `insights.air-local`. You need to add the corresponding entry to your `/etc/hosts`.
- The certificate in `air/router/dev_cert/aircloak.com.chain.pem` has been changed. You need to delete the old key from your keychain, and import the new version as explained [here](air/README.md#running-the-system-on-the-localhost).
- The new site is now accessed through the router. You need to start air dependencies, and then the site will be available at https://insights.air-local:20000
