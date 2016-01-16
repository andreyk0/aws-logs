build:
	stack build

build-image: build rootfs.tar
	docker build -t andreyk0/aws-logs:latest .

publish-image: build-image
	docker push andreyk0/aws-logs:latest

rootfs.tar:
	tar cvf $@ --exclude private /etc/ssl

tags:
	hasktags -R -x -c .sources src

clean:
	rm -f rootfs.tar
	stack clean

.PHONY: build tags clean
