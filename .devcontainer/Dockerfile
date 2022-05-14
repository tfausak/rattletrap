FROM haskell:9.2.2-slim
ARG USER=haskell
RUN useradd --create-home --shell "$( which bash )" "$USER"
USER "$USER"
ENV PATH="/home/$USER/.cabal/bin:$PATH"
