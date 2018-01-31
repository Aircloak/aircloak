all: build

build:
	mkdir -p _build/macos
	mkdir -p _build/win
	GOOS=darwin GOARCH=amd64 go build -o _build/macos/batch_queries batch_queries.go
	GOOS=windows GOARCH=386 go build -o _build/win/batch_queries.exe batch_queries.go
