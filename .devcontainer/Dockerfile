FROM haskell:8.10.4
ARG USER=haskell
RUN useradd --create-home --shell "$( which bash )" "$USER"
USER "$USER"
ENV PATH="/home/$USER/.cabal/bin:$PATH"
