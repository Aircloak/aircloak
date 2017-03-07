.PHONY: all compile deps release

release_version = `cat ../VERSION`

all: deps compile

compile:
	mix compile

deps:
	mix deps.get

release: deps compile
	tar -czvf /tmp/aircloak_guides-$(release_version).tar.gz -C _build guides
