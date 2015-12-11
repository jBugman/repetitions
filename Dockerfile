FROM haskell:7.10
MAINTAINER Sergey Parshukov <codedby@bugman.me>

RUN cabal update

ADD . /opt/build

RUN cd /opt/build && cabal install --only-dependencies

ENTRYPOINT cd /opt/build && cabal build
