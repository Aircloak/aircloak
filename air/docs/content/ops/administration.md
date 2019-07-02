# Administration

## Forgotten password

### Reset by link

In case a user has forgotten their password the system administrator can generate a password reset link. The option
is available by navigating to `Admin -> Users` and clicking `Edit` by the appropriate user.

### Reset from the command line

It's also possible to generate a password reset token from the command line on the machine where the Insights Air image
is running. This option can be used to reset a lost password even if no admin accounts are accessible. To do it, issue
the following command:

```
$ docker exec -it [container] bin/air reset_password [login]
```

In the command above, substitute the name of your Insights Air container (most likely `air`) for `[container]`, and
the login of the user for whom you want to reset the password for `[login]`. As a result you will get output like the
following:

```
Use the following token in the `Forgot password` form:

SFMyNTY.g3QAAAACZAAEZGF0YWECZAAGc2lnbmVkbgYAWanyP2MB.Sh__XEigzPuzGsE0tN79Hxwgnuy-izegy-4RKzbQlAY

ok
```

Next, click on `Forgot password?` in the Airclok Insights login form, and follow the on-screen instructions.

## User sessions

When a user signs in to the Insights Air web frontend, a session cookie is placed in the user's browser that is later
used to authenticate the user without the need to sign in for every operation. In the event of the user's account being
compromised, such a cookie could theoretically be used to access the user's account even after a password change. A
similar situation might occur if the user signs in from a device that they later lose control of, like a publicly
available computer, and they don't sign out.

To remedy this problem it's possible to sign out all existing sessions for the given user. A user can do that themselves
by navigating to `Settings` and clicking `Sign out other sessions`. Alternatively, an Insights Air administrator can
sign out all sessions for any user by navigating to `Admin -> Users -> Edit` and clicking `Sign out all sessions`. Also
note, that when using the reset password funcionality all sessions are signed out automatically.

## Analysis status

A section in the admin panel available under `Admin -> Analysis` provides an overview of the status of analysis queries
performed by Insights Cloak instances connected to the given Insights Air. See [the section on column
analysis](/sql/restrictions.md#column-analysis) for more on why this analysis is done. You can group the data by table,
host or data source by clicking on the tabs on top. For each group the following values will be listed:

- Columns - total number of database columns in the given group
- Analyzed - number of columns for which all aspects of the analysis have completed successfully
- Processing - number of columns that still require at least one aspect of the analysis to complete
- Failed - number of columns for which at least one of the aspects of analysis failed unexpectedly (for example because
  of a database timeout)
- Isolators/Rare values/Bounds - there are three numbers under each of these headings separated by `/`. The values
  represent the number of successful, pending, and failed column analyses, respectively.

Note that columns for which an aspect of analysis failed will be treated as not yet analyzed in that respect. This will
put some additional restrictions on their usage in queries, as described in [the section on column
analysis](/sql/restrictions.md#column-analysis).
