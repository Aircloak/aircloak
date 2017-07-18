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
