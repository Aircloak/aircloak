nginx
=====

----------------------

- [What it does](#what-it-does)
- [Getting start](#getting-started)

----------------------

# What it does

This folder contains an nginx configuration that is used during local development
in order to have both the frontend and backend applications behind the same HTTP
end-point.

This is a requirement for them to share the same user authentication, as the authentication
works through session cookies which are unique to the particular domain.

# Getting started

The best way of getting started is by using the `start-support.sh` script
in the top-level directory. It basically starts the nginx as `nginx -c $PWD/nginx.conf`.
