#!/usr/bin/env bash

set -eux

TAG=$(git rev-parse --short HEAD)

rm -rf docker/app
mkdir -p docker/app
stack build
cp $(stack exec which yesodweb) docker/app
cp -r static config docker/app

(
cd docker
docker build -t snoyberg/yesodweb:$TAG .
docker push snoyberg/yesodweb:$TAG
)
