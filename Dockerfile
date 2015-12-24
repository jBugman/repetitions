FROM haskell:7.10
MAINTAINER Sergey Parshukov <codedby@bugman.me>

RUN cabal update

ADD . /opt/build

WORKDIR /opt/build

RUN cabal install --only-dependencies -j2

CMD ["cabal", "build"]
