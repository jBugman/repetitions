build-deps:
	docker build --rm=true -t jbugman/repetitions .

build:
	docker run -v `pwd`/dist:/opt/build/dist --rm jbugman/repetitions

deploy:
	cp dist/build/repetitions/repetitions ~/Dropbox/Apps/Heroku/repetitions/bin/repetitions
