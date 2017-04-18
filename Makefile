TARGET=target
EXE=$(TARGET)/aws-logs
DIST_EXE=$(EXE)-$(shell uname -s)-$(shell uname -m)
DIST_EXE_SIG=$(DIST_EXE).sig

build:
	stack build aws-logs

build-prof:
	stack build --profile --ghc-options="-rtsopts" aws-logs

install:
	stack install aws-logs

bindist:
	mkdir -p $(TARGET)
	stack --local-bin-path $(TARGET) install $(STACK_OPTS) aws-logs
	upx --best $(EXE)
	mv $(EXE) $(DIST_EXE)
	gpg --output $(DIST_EXE_SIG) --detach-sign $(DIST_EXE)

clean:
	stack clean
	rm -rf target

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies


.PHONY: build build-prof clean tags sources

