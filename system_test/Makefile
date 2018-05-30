SHELL=/bin/bash -o pipefail

.PHONY: all compile deps test dev-container

all: deps compile

compile:
	MIX_ENV=test mix compile

deps:
	mix deps.get

test:
	mix test

dev-container:
	../ci/start_component.sh system_test compile system_test
