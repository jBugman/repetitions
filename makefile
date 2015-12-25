build-deps:
	docker build --rm=true -t jbugman/repetitions .

build:
	mv cabal.sandbox.config ~cabal.sandbox.config
	rm -rf dist
	docker run -v `pwd`:/opt/build --rm jbugman/repetitions
	mv ~cabal.sandbox.config cabal.sandbox.config

fix-build:
	mv ~cabal.sandbox.config cabal.sandbox.config

deploy:
	mkdir -p ~/Dropbox/Apps/Heroku/repetitions/bin
	cp dist/build/repetitions/repetitions ~/Dropbox/Apps/Heroku/repetitions/bin/repetitions
