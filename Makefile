all:
	stack test

file:
	stack build && stack exec salty-exe && cat test.php

debug:
	stack build && stack exec salty-exe debug
