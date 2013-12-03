MIX=`which mix`
.DEFAULT_GOAL := build
.PHONY: build test clean

build:
	$(MIX) deps.get
	$(MIX) compile --all --force

test:
	@MIX_ENV=test $(MIX) do deps.get, test --no-start

clean:
	rm -rf ebin
	rm -rf deps
	rm -rf _build
