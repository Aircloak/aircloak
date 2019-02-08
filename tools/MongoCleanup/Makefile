.PHONY: run publish publish-osx publish-linux reset elixir-deps

run: reset-db
	dotnet run --project MongoCleanup -- --cloak-config teambank_mongo_34.json

generate-data: elixir-deps
	mongo mongo_cleanup_big --eval "printjson(db.dropDatabase())"
	mix run data_generator.exs
	mongoimport -d mongo_cleanup_big -c users --file=/tmp/users.json
	mongoimport -d mongo_cleanup_big -c purchases --file=/tmp/purchases.json
	mongoimport -d mongo_cleanup_big -c items --file=/tmp/items.json

elixir-deps:
	mix deps.get

reset-db:
	mongo teambank_konto_service --eval "printjson(db.dropDatabase())"
	mongorestore --gzip --archive=teambank_dump.gz

publish: publish-osx publish-linux

publish-osx:
	dotnet publish -c Release -r osx-x64 --self-contained true MongoCleanup

publish-linux:
	dotnet publish -c Release -r linux-x64 --self-contained true MongoCleanup
