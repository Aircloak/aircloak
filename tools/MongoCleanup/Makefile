.PHONY: run publish publish-osx publish-linux

run:
	dotnet run --cloak-config teambank_mongo_34.json

publish: publish-osx publish-linux

publish-osx:
	dotnet publish -c Release -r osx-x64 --self-contained true

publish-linux:
	dotnet publish -c Release -r linux-x64 --self-contained true
