.PHONY: run publish publish-osx publish-linux

run: reset-db
	dotnet run --project MongoCleanup -- --cloak-config teambank_mongo_34.json

reset-db:
	mongo teambank_konto_service --eval "printjson(db.dropDatabase())"
	mongorestore --gzip --archive=teambank_dump.gz

publish: publish-osx publish-linux

publish-osx:
	dotnet publish -c Release -r osx-x64 --self-contained true

publish-linux:
	dotnet publish -c Release -r linux-x64 --self-contained true
