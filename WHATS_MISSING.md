The goal isn't to have every PHP feature in here, but there are definitely some notable things missing right now that I'd like to add sometime.

- try/catch statements
- pass by reference (`foo = &bar`)
- function arg defaults (foo a=1 := a + 1)

chaining optionals for hash keys like

    :foo.bar?.baz?

would be cool.

Use php 7.4 lambda shorthand to allow HoFs to be passed into functions:

$b = array_map(fn($n) => $n * $n * $n, $a);
print_r($b);

@ should parse to $this so

    return @

can work.

fails:
    foo := {
        [
            ["", "en", "US", "USD", false, false, false, []],
        ]
    }

Phan needs a variable name for type annotations...

     * @param bool $value

instead of

     * @param bool

name not needed for returns, or for annotations on variables

Stuff I don't care about:
- switch statements (though guards for functions like haskell would be cool)
