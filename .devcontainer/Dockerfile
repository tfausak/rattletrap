FROM haskell:9.0.1
ARG USER=haskell
RUN useradd --create-home --shell "$( which bash )" "$USER"
USER "$USER"
ENV PATH="/home/$USER/.cabal/bin:$PATH"
