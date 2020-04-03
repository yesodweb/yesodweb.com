FROM docker.pkg.github.com/yesodweb/yesodweb.com/base-build:880b1ff1eb3db055885024974dd67bdb8c0e2484 as build-app

RUN mkdir -p /artifacts/bin
COPY . /src
RUN stack install --stack-yaml /src/stack.yaml --local-bin-path /artifacts/bin

FROM docker.pkg.github.com/yesodweb/yesodweb.com/base-run:880b1ff1eb3db055885024974dd67bdb8c0e2484

ENV PORT 3000
WORKDIR /app
CMD ["/usr/local/bin/yesodweb", "production"]

COPY --from=build-app /artifacts/bin/yesodweb /usr/local/bin
COPY --from=build-app /src/static /app/static
COPY --from=build-app /src/config /app/config
