all:
	stack test

test:
	stack test

file:
	stack build && stack exec salty && cat test.php

debug:
	stack build && stack exec salty debug

run:
	stack build && stack exec salty && php test.php 3
