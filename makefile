prebuild:
	docker build -t jbugman/repetitions .

build:
	docker run jbugman/repetitions
