FROM ubuntu:14.04

RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y libgmp10 git

ADD app /app
WORKDIR /app

ENTRYPOINT ./yesodweb production
EXPOSE 3000
