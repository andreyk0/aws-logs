build:
	stack build

tags:
	hasktags -R -x -c .sources src

clean:
	stack clean

.PHONY: build tags clean
