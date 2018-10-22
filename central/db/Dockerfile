FROM postgres:9.5

RUN apt-get update && apt-get install jq -y
COPY central/db/init_db.sh /docker-entrypoint-initdb.d/init_db.sh
