# Ahead of time configuration of Insights Air

Aircloak Insights allows for hands-off automated deployments using orchestration tools such as
[Kubernetes](https://kubernetes.io/). To avoid having to complete the setup process in a web browser,
you can fully configure the system in the Insights Air `config.json` file.

## Aircloak Insights privacy policy

In some jurisdictions it is a legal requirement to provide a privacy policy on any website accessible from the internet.
The privacy policy can be provided under the `privacy_policy_file` key of the `site` object in the Insights Air
`config.json` file. The file containing the privacy policy must be readable from within the Insights Air container
(see the [File permissions](/ops/configuration.md#file-permissions) section for more information on the required permissions) and should reside in the same or a subfolder of the folder in which the `config.json` file is stored. The file can contain markdown formatting.
Preconfiguring the privacy policy does not prevent you from later updating it through the web interface.

```
$ ls -la
-rw-r--r--   1 user  staff   898 Jan 1 08:00 config.json
-rw-r--r--   1 user  staff   889 Jan 1 08:00 privacy-policy.md

$ cat privacy-policy.md
Privacy policy
==============

This privacy policy outlines what data is being collected about you
when you are using Aircloak Insights. It also outlines your rights
and who you should reach out to if you have questions or concerns.

...
```

Given a setup as shown above, the `config.json` file would look like this:

```json
{
  "site": {
    ...
    "privacy_policy_file": "privacy-policy.md"
  },
  ...
}
```

## Users and data sources

Aircloak Insights allows you to preconfigure a set of users and data sources. The user accounts, data sources and information
about which user accounts should have access to which data sources can be configured as a JSON object in a file available
under the `users_and_datasources_file` key of the `site` object in the Insights Air `config.json` file.
The users and data source definitions file must be readable from within the Insights Air container (see the [File permissions](/ops/configuration.md#file-permissions) section for more information on the required permissions) and should reside in the same or a subfolder of the folder in which the `config.json` file is stored.

```
$ ls -la
-rw-r--r--   1 user  staff   898 Jan 1 08:00 config.json
-rw-r--r--   1 user  staff   889 Jan 1 08:00 users-and-datasources.json
```

The file should take the following shape:

```json
{
  "users": [
    ... user account definitions
  ],
  "data_sources": [
    ... data source definitions
  ]
}
```

where the `users` key is required, and the `data_sources` key optional.

### Users

The user account definitions take the form of:

```json
{
  "login": string,
  "password_hash": string,
  "admin": boolean
}
```

Where `login` is the desired login the user account should be given,
the `password_hash` is a hashed version of the user accounts plain text password,
and `admin` is an optional boolean field. If set to `true`, the user will be given
administrative privileges.

Aircloak provides a utility for converting plain text passwords into a hashed form that is usable by Insights Air.
This utility is part of the Insights Air docker container itself. The utility requires that you type the passwords
in plaintext on the command line. Care must be taken to ensure that the passwords are not recorded in the command
line history! This would compromise the security of the accounts.
[The following article](https://stackoverflow.com/questions/640403/can-you-prevent-a-command-from-going-into-the-bash-shell-command-history)
explains measures that can be taken to ensure the command is not recorded.

You can run the Aircloak-provided utility as follows:

```
$ docker run -it quay.io/aircloak/air:VERSION bin/air hash_passwords password1 password2
$pbkdf2-sha512$160000$PIgtdVtTBBD9CHyPKNfWCA$sLZLmn2mJoF5ztebglec0F5b11YOuUPO3OipUf8EziOq6fQNxFNeIg8YmMVQhP5YrI4Fai.w2IObrUxCwYWhxA
$pbkdf2-sha512$160000$UZgmmyvwYGEAaFNzhJLx8w$D4IMDQDoEh39V5NiQ8QarbWcK.KWKdIQXxbuXIiqbQshJ6suZdlYO.7keC/9GxaeyXSvdaN9RJv.g7QNsV4RYA
```

where `VERSION` is the version of the Insights Air system you are running.

### Data sources

The data source definitions take the form of:

```json
{
  "name": string,
  "logins": string array,
  "group_name": string
}
```

Where `name` is the data source name. It must be provided _exactly_
as it is configued using the `name` parameter in the Insights Cloak data source configuration.
`logins` should be an array of the `login`s of user accounts that should be given access to the data source.
These accounts need exist in the system (for example having been preconfigured using the `users`
field) before the data source is created.
`group_name` is the desired name of the group that the users and data source should belong to.
It can generally be set to the name of the data source itself, but is not allowed to be the same as the name
of an existing group.

### Practical example

In this example we will configure user accounts for two users: Alice and Bob.
Alice should be given administrative privileges, and both users should have access to query
a data source named `BobCorpData`. Additionally Alice should be given access to query a
data source called `InternalStats`.

#### 1. Hash the account passwords

Using the Aircloak Insights utility ([ensuring plaintext passwords are not recorded in the
command line history](https://stackoverflow.com/questions/640403/can-you-prevent-a-command-from-going-into-the-bash-shell-command-history)),
we hash the passwords of `Alice` and `Bob`:

```
$ docker run -it quay.io/aircloak/air:VERSION bin/air hash_passwords AlicePassword BobPassword
$pbkdf2-sha512$160000$PIgtdVtTBBD9CHyPKNfWCA$sLZLmn2mJoF5ztebglec0F5b11YOuUPO3OipUf8EziOq6fQNxFNeIg8YmMVQhP5YrI4Fai.w2IObrUxCwYWhxA
$pbkdf2-sha512$160000$UZgmmyvwYGEAaFNzhJLx8w$D4IMDQDoEh39V5NiQ8QarbWcK.KWKdIQXxbuXIiqbQshJ6suZdlYO.7keC/9GxaeyXSvdaN9RJv.g7QNsV4RYA
```

#### 2. Create a users and data sources definition

Once we have the password hashes available, we can create the users and data sources configuration file.
In this example we will call it `users_and_datasources.json`. The configuration file must be stored alongside
the Insights Air configuration file or in another folder accessible by Insights Air.

```json
$ ls -la
-rw-r--r--   1 user  staff   898 Jan 1 08:00 config.json
-rw-r--r--   1 user  staff   889 Jan 1 08:00 users_and_datasources.json

$ cat users_and_datasources.json
{
  "users": [
    {
      "login": "alice",
      "password_hash": "$pbkdf2-sha512$160000$PIgtdVtTBBD9CHyPKNfWCA$sLZLmn2mJoF5ztebglec0F5b11YOuUPO3OipUf8EziOq6fQNxFNeIg8YmMVQhP5YrI4Fai.w2IObrUxCwYWhxA",
      "admin": true
    },
    {
      "login": "bob",
      "password_hash": "$pbkdf2-sha512$160000$UZgmmyvwYGEAaFNzhJLx8w$D4IMDQDoEh39V5NiQ8QarbWcK.KWKdIQXxbuXIiqbQshJ6suZdlYO.7keC/9GxaeyXSvdaN9RJv.g7QNsV4RYA"
    }
  ],

  "data_sources": [
    {
      "name": "BobCorpData",
      "logins": ["alice", "bob"],
      "group_name": "BobCorpData"
    },
    {
      "name": "InternalStats",
      "logins": ["alice"],
      "group_name": "InternalStats-access-group"
    }
  ]
}
```

#### 3. Amend the Insights Air configuration file

The Insights Air configuration file (`config.json`) needs to be amended to include a reference to the
`users_and_datasources.json` file. Once amended it will look as follows:

```json
{
  "site": {
    ...
    "users_and_datasources_file": "users_and_datasources.json"
  },
  ...
}
```

Upon starting up, Insights Air will configure the accounts for Alice and Bob and pre-create data source
scaffolds for the two data sources. Alice's account will have access to query both data sources,
whereas Bob's account will only have access rights to the `BobCorpData` data source.
