FROM ubuntu:14.04

ENV HOME /app
ENV LANG en_US.UTF-8
RUN locale-gen en_US.UTF-8
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y libgmp10 git

ADD dist/build/yesodweb/yesodweb /app/yesodweb
ADD static /app/static
ADD config /app/config

RUN chown -R nobody /app

WORKDIR /app
CMD ["/app/yesodweb", "production"]
