FROM postgres:12.1

RUN apt-get update && apt-get install -y jq
COPY db/init_db.sh /docker-entrypoint-initdb.d/init_db.sh
