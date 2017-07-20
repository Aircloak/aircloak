This guide assumes you're using `homebrew` and `asdf`.

## Installing ODBC

```
brew install unixodbc
brew install psqlodbc
```

Then, from this folder run:

```
odbcinst -i -d -l -f priv/odbc/osx/odbcinst.ini && odbcinst -i -s -l -f priv/odbc/osx/odbc.ini
```

To verify that everything is fine:

```
$ isql -v PostgreSQL

SQL> select 1
```

## Erlang with ASDF

You might have to run `xcode-select --install` in order to get erlang to install.
For more context, please check out [this stackoverflow
reply](https://stackoverflow.com/questions/39236691/building-otp-18-3-on-os-x-10-9-odbc-library-header-check-failed/40184770#40184770).

If the install fails, please ensure your `asdf` and `asdf-erlang`-plugin are recent and up to date.
