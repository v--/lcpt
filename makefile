.PHONY: test

test ?= $(wildcard exercises/*)

test:
	for file in $(test); do runhaskell $$file; done
