SHELL=/bin/bash -o pipefail

.PHONY: dev-container

all: deps compile

compile:
	mix compile

deps:
	mix deps.get

dev-container:
	../ci/start_component.sh system_test
