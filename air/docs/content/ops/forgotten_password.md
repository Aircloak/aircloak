# Forgotten password

## Reset by link

In case a user has forgotten their password the system administrator can generate a password reset link. The option
is available by navigating to Admin -> Users and clicking "Reset password" by the appropriate user.

## Reset from the command line

It's also possible to generate a password reset token from the command line on the machine where the Insights Air image
is running. This option can be used to reset a lost password even if no admin accounts are accessible. To do it, issue
the following command:

```
$ docker exec -it [container] bin/air reset_password [email]
```

In the command above, substitute the name of your Insights Air container (most likely `air`) for `[container]`, and
the email of the user for whom you want to reset the password for `[email]`. As a result you will get output like the
following:

```
Use the following token in the `Forgot password` form:

SFMyNTY.g3QAAAACZAAEZGF0YWECZAAGc2lnbmVkbgYAWanyP2MB.Sh__XEigzPuzGsE0tN79Hxwgnuy-izegy-4RKzbQlAY

ok
```

Next, click on `Forgot password?` in the Airclok Insights login form, and follow the on-screen instructions.
