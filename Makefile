emacs ?= emacs

ert_tests_el = $(wildcard test/*.el)
el = $(wildcard *.el)
elc = $(el:%.el=%.elc)

.PHONY: all test test-ert build

all:

test: build test-ert

build: $(elc)

$(elc): %.elc: %.el
	$(emacs) -batch -Q -f batch-byte-compile $<

test-ert: $(ert_tests_el)
	$(emacs) -batch -Q -L . --eval "(require 'ert)" $(^:%=-l "%") \
		-f ert-run-tests-batch-and-exit
