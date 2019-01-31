publish-linux:
	dotnet publish -c Release -r linux-x64 --self-contained true

publish-osx:
	dotnet publish -c Release -r osx-x64 --self-contained true

publish: publish-osx publish-linux

start:
	dotnet run -p BMODataGenerator/BMODataGenerator.fsproj 10 $(PWD)/test.json
