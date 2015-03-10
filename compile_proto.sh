#!/usr/bin/env bash
set -e

mkdir -p lib/proto/air
for p in proto/aircloak/air/*.proto; do
  protoc --beefcake_out lib/proto/air -I proto $p
done
