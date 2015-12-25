build-deps:
	docker build --rm=true -t jbugman/repetitions .

build:
	mv cabal.sandbox.config ~cabal.sandbox.config
	docker run -v `pwd`:/opt/build --rm jbugman/repetitions
	mv ~cabal.sandbox.config cabal.sandbox.config

deploy:
	cp dist/build/repetitions/repetitions ~/Dropbox/Apps/Heroku/repetitions/bin/repetitions
