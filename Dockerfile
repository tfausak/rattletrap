FROM alpine:3.12.2
RUN apk add --no-cache cabal ghc gmp-dev musl-dev wget zlib-dev
WORKDIR /root/rattletrap
COPY . .
RUN cabal update
RUN cabal build --flags static
RUN cabal exec -- sh -c 'cp $( which rattletrap ) /usr/local/bin/'

FROM alpine:3.12.2
COPY --from=0 /usr/local/bin/rattletrap /usr/local/bin/
CMD rattletrap
