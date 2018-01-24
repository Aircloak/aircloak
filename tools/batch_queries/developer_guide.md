To run the tool locally, you need to install go with `asdf install`.

You first need to configure the tool, as explained in `user_guide.md`. Then, you can start it with `go run batch_queries.go`.

To produce a standalone binary for Windows you can invoke the following command:

```
GOOS=windows GOARCH=386 go build batch_queries.go
```

For other possible build targets see [here](https://www.digitalocean.com/community/tutorials/how-to-build-go-executables-for-multiple-platforms-on-ubuntu-16-04#step-4-%E2%80%94-building-executables-for-different-architectures).
