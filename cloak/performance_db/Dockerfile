FROM postgres:11.1

RUN \
  apt-get update && \
  apt-get install -y git libprotobuf-c0-dev make postgresql-server-dev-11 protobuf-c-compiler gcc

RUN \
  git clone https://github.com/citusdata/cstore_fdw.git && \
  cd cstore_fdw && \
  PATH=/usr/local/pgsql/bin/:$PATH make -j8 && \
  PATH=/usr/local/pgsql/bin/:$PATH make install

COPY cloak/performance_db/postgresql.conf /etc/postgresql/postgresql.conf

RUN \
  echo "CREATE EXTENSION cstore_fdw;" > /docker-entrypoint-initdb.d/init_cstore_fdw.sql && \
  echo "CREATE SERVER cstore_server FOREIGN DATA WRAPPER cstore_fdw;" >> /docker-entrypoint-initdb.d/init_cstore_fdw.sql
