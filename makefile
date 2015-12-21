init:
	docker build --rm=true -f docker/Init -t jbugman/repetitions .

deps:
	docker build --rm=true -f docker/Build -t jbugman/repetitions .

build:
	docker run -v `pwd`/dist:/opt/build/dist --rm jbugman/repetitions

deploy:
	cp dist/build/repetitions/repetitions ~/Dropbox/Apps/Heroku/repetitions/bin/repetitions
