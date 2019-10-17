FROM alpine:3.10.2
RUN apk add --no-cache cabal ghc gmp-dev musl-dev wget zlib-dev
WORKDIR /root/rattletrap
COPY . .
RUN cabal v2-update
RUN cabal v2-build --flags static
RUN cabal v2-exec -- sh -c 'cp $( which rattletrap ) /usr/local/bin/'

FROM alpine:3.10.2
COPY --from=0 /usr/local/bin/rattletrap /usr/local/bin/
CMD rattletrap
