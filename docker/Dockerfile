FROM ubuntu:20.04
RUN apt-get update && apt-get install --assume-yes ca-certificates
COPY rattletrap /usr/local/bin
RUN chmod +x /usr/local/bin/rattletrap
