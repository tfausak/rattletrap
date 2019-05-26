FROM alpine:3.9.4 AS build
RUN apk add --no-cache cabal ghc gmp-dev musl-dev wget zlib-dev
RUN cabal update
WORKDIR /root/rattletrap
COPY . .
RUN cabal new-build --flags static
RUN cp $( cabal new-exec which rattletrap | tail -n 1 ) /usr/local/bin/

FROM alpine:3.9.4
COPY --from=build /usr/local/bin/rattletrap /usr/local/bin/rattletrap
CMD rattletrap
