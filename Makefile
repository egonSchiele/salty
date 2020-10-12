all:
	stack test

file:
	stack exec salty-exe && cat test.php
