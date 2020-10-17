.PHONY: all test debug run

all:
	stack test

test:
	stack test 2> out && less out

file:
	stack build && stack exec salty test.salt && cat test.php

debug:
	stack build && stack exec salty debug

run:
	stack build && stack exec salty test.salt && php test.php 3

install:
	stack build --copy-bins
