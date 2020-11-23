# ----------------------------------------------------------------------------

FROM heroku/heroku:16-build as build

# ----------------------------------------------------------------------------

LABEL maintainer="daro@darographix.de"
LABEL version="1.0"
LABEL description="The Glorious Haskell Bot Dockerfile for Heroku"

# ----------------------------------------------------------------------------

# Stack stores binaries in /root/.local/bin
ENV PATH /root/.local/bin:$PATH

# Heroku assumes we'll put everything in /app/user
ENV HOME /app/user
RUN mkdir -p $HOME
WORKDIR $HOME

# We need to set the LANG for the linux wrapper used by heroku to some UTF-8
ENV LANG en_US.UTF-8


RUN curl -sSL https://get.haskellstack.org/ | sh

#13 Use system GHC
RUN stack config set system-ghc --global true


# Preinstall GHC using Stack
ENV STACK_LTS_VERSION 16.23

RUN stack setup  --resolver lts-$STACK_LTS_VERSION
# Install application framework in a separate layer for caching
COPY ./stack-bootstrap .

RUN stack install \
  --resolver lts-$STACK_LTS_VERSION \
  --ghc-options="-XRebindableSyntax -Wno-deprecations -Wno-pointer-to-int-cast" \
  $(cat stack-bootstrap)

# Copy over configuration for building the app
COPY stack.yaml .
COPY *.cabal .

#18 Build dependencies so that if we change something later we'll have a Docker
#   cache of up to this point.
RUN stack build --ghc-options="-XRebindableSyntax -Wno-deprecations -Wno-pointer-to-int-cast" --dependencies-only
COPY src ./src
COPY auth-token.secret ./auth-token.secret
COPY ChangeLog.md ./ChangeLog.md
COPY LICENSE ./LICENSE

# Run pre-build script if it exists (compile CSS, etc)
RUN if [ -x bin/pre-build ]; then bin/pre-build; fi

#21 Build and copy the executables into the app
RUN stack --local-bin-path=. install


# ---------------------------------------------------------------------------- #

FROM heroku/heroku:16

# ---------------------------------------------------------------------------- #

# Started a new wrapper getting rid of everything ...
ENV HOME /app/user
RUN mkdir -p $HOME
WORKDIR $HOME

#27 Restore files from previous wrapper
COPY --from=build $HOME/haskell-bot .

# Load config vars via .env file. Assume were always on production in docker container.
COPY ./production.env $HOME/.env

RUN . $HOME/.env

RUN apt-get update && apt-get install -y \
  vim

# ---------------------------------------------------------------------------- #

ENV PATH $HOME:$PATH
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8
ENV LC_ALL en_US.UTF-8
RUN . $HOME/.env && echo "Installation complete."
CMD ["haskell-bot"]
