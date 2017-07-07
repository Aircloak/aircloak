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

## Installing Erlang

You need to reinstall Erlang and make it link to ODBC. At the time of writing this, Erlang 18 seems to have a hardcoded link to `iodbc`, so some hackery is required to make it work with `unixodbc`. If you're able to solve the problem in a nicer way, feel free to update this guide :-)

Assuming you're using `asdf`, you first need to uninstall Erlang:

```
asdf uninstall erlang 19.2
```

Next, you need to fetch Erlang source:

```
cd /tmp
wget http://erlang.org/download/otp_src_19.2.tar.gz && tar xzvf otp_src_19.2.tar.gz
cd otp_src_19.2
```

Now comes the hacky part. Open the entire folder in editor, search for `iodbc`, and replace the occurrences with `odbc`. The changes should be in `lib/odbc/configure` and `lib/odbc/configure.in`.

Now you can build Erlang and install it to `~/.asdf` folder:

```
./configure \
  CFLAGS="-I/usr/local/opt/unixodbc/include" \
  LDFLAGS="-L/usr/local/opt/unixodbc/lib -lodbc" \
  --prefix=$HOME/.asdf/installs/erlang/19.2/ \
  --disable-hipe \
  --with-odbc=/usr/local/opt/unixodbc \
  --with-ssl=/usr/local/opt/openssl

make && make install
```

If the configure fails, make sure the paths are correct for your own system.
You can check the paths for `--with-odbc` and --`with-ssl` with the following two commands
`brew --prefix unixodbc` and `brew --prefix openssl` respectively.
To get the `CFLAGS` and `LDFLAGS` path right, ensure that the version number also matches
the one you have installed.

Assuming you have setup local `cloak` database, you can verify that all is fine:

```
$ erl

1> application:start(odbc), odbc:connect("DSN=PostgreSQL;UID=cloak;Database=cloak;Server=localhost;", []).
```

At this point, Erlang is properly installed, and managed by `asdf`. If you already had Elixir installed, you don't need to reinstall it.
