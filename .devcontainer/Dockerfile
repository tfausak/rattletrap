ARG UBUNTU_VERSION=20.04
FROM ubuntu:"$UBUNTU_VERSION"

ENV LANG=C.UTF-8
RUN apt-get update && apt-get install --assume-yes clang curl git libgmp-dev make sudo zlib1g-dev

ARG USER_NAME=haskell
RUN useradd --create-home --shell "$( which bash )" "$USER_NAME"
RUN echo "$USER_NAME ALL=(ALL) NOPASSWD: ALL" | tee "/etc/sudoers.d/$USER_NAME"
USER "$USER_NAME"
WORKDIR "/home/$USER_NAME"
ENV PATH="/home/$USER_NAME/.cabal/bin:/home/$USER_NAME/.ghcup/bin:$PATH"

ARG GHCUP_VERSION=0.1.17.4
RUN mkdir --parents ~/.ghcup/bin
RUN curl --output ~/.ghcup/bin/ghcup "https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/$( uname -m )-linux-ghcup-$GHCUP_VERSION"
RUN chmod +x ~/.ghcup/bin/ghcup
RUN ghcup --version

ARG GHC_VERSION=8.10.7
RUN ghcup install ghc "$GHC_VERSION" --set
RUN ghc --version

ARG CABAL_VERSION=3.6.2.0
RUN ghcup install cabal "$CABAL_VERSION" --set
RUN cabal --version

ARG HLS_VERSION=1.6.1.0
RUN ghcup install hls "$HLS_VERSION" --set
RUN haskell-language-server-wrapper --version
