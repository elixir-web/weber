MIX=`which mix`
.DEFAULT_GOAL := build
.PHONY: build test clean

build:
	$(MIX) deps.get
	$(MIX) compile

test:
	@MIX_ENV=test $(MIX) do deps.get, test

clean:
	rm -rf ebin
	rm -rf deps
