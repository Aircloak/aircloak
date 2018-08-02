FROM postgres:9.4

RUN apt-get update && apt-get install -y jq
COPY air/db/init_db.sh /docker-entrypoint-initdb.d/init_db.sh
