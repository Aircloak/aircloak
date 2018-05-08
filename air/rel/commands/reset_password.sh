#!/bin/bash

shift || true

$RELEASE_ROOT_DIR/bin/air eval "'Elixir.Air.ReleaseCLI':reset_password(\"$1\")"
