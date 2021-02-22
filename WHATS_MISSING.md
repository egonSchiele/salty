The goal isn't to have every PHP feature in here, but there are definitely some notable things missing right now that I'd like to add sometime.

- try/catch statements
- pass by reference (`foo = &bar`)
- function arg defaults (foo a=1 := a + 1)

would be cool.

Use php 7.4 lambda shorthand to allow HoFs to be passed into functions:

$b = array_map(fn($n) => $n * $n * $n, $a);
print_r($b);

Phan needs a variable name for type annotations...

     * @param bool $value

instead of

     * @param bool

name not needed for returns, or for annotations on variables
