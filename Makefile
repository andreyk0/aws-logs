build:
	stack build

build-image: build
	docker build -t andreyk0/aws-logs:latest .

tags:
	hasktags -R -x -c .sources src

clean:
	stack clean

.PHONY: build tags clean
