all:
	stack test

file:
	stack build && stack exec salty && cat test.php

debug:
	stack build && stack exec salty debug
