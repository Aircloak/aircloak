#!/usr/bin/env bash
set -e

for p in proto/aircloak/air/*.proto; do 
  protoc --beefcake_out lib/proto/air -I proto $p
done

protoc --beefcake_out lib/proto/cloak -I proto proto/aircloak/cloak/query.proto
