# salty

## Usage
Compile the code yourself or just copy one of the binaries in `bin`.

## Compiling

Install Stack:

https://docs.haskellstack.org/en/stable/install_and_upgrade/

Avoid piping a script to `sh`. Then:

    make install

Then:

```
printHelp = do
    putStrLn "Usage: `salty test.salt` prints to stdout"
    putStrLn "Usage: `salty` reads from stdin and prints to stdout"
    putStrLn "Usage: `salty debug <filename>` reads file and prints tree to stdout"
```

## Variables

- `foo` becomes `$foo`
- `@foo` becomes `$this->foo`
- `@@foo` becomes `static::$foo`

## Consts

Inside a class,

```
FOO = 1
_FOO = 1
@@FOO = 1
```

becomes

```
public const FOO = 1;
private const FOO = 1;
static::FOO = 1;
```

outside a class, you need `const`:

```
const FOO = 1
```

becomes

```
const FOO = 1;
```

You can use const in classes too. In fact you could write:

```
class Foo {
    public static const FOO = 1
}
```

Which would become

```
class Foo {
    public static const FOO = 1;
}
```

## Functions

```
foo a b := a + b
```

becomes

```
public function foo($a, $b) {
    return $a + $b;
}
```
(note the implicit return)

### Multi-line functions:

```
foo a b := {
  bar = 1
  baz = a
  return bar + baz
}
```

becomes

```
public function foo($a, $b) {
    $bar = 1;
    $baz = $a;
    return $bar + $baz;
}
```

### Static functions:

```
@@build a b := return 2
```

becomes

```
public static function build($a, $b) {
    return 2;
}
```

### Private function:

```
_foo a b := a + b
```

becomes

```
private function foo($a, $b) {
    return $a + $b;
}
```

### Type signatures

```
foo :: string -> string
foo a := a
```

becomes

```
/**
 * @param string
 * @return string
 */
public function foo(string $a) {
    return $a;
}
```

```
foo :: string? -> string
foo a := a
```

becomes

```
/**
 * @param string|null
 * @return string
 */
public function foo(?string $a = null) {
    return $a;
}
```

```
foo :: string -> int? -> string
foo a b := a
```

becomes

```
/**
 * @param string|null
 * @param int
 * @return string
 */
public function foo(?string $a = null, int $b) {
    return $a;
}
```

Variable type signatures:

    var :: string
    myVar = "hi"

becomes:

    /** @var string */
    $myVar = "hi";

and

    var :: string?
    myVar = null

becomes:

    /** @var string|null */
    $myVar = null;
## Hash creation

```
a = {
  foo: 1,
  bar: 2,
  baz: 'hi',
}
```

becomes

```
$a = [
    "foo" => 1,
    "bar" => 2,
    "baz" => "hi"
]
```

Trailing comma is optional.

Note empty hash `{}` is not allowed, you need to make it like this: `[]`. See EDGE_CASES.md for an explanation of why.

## Dot notation

```
:foo.bar.baz
```

becomes

```
$foo["bar"]["baz"];
```

## If Statement

```
if a != 'foo' then return 2 else return 3
```

becomes

```
if ($a != "foo") {
    return 2;
} else {
    return 3;
}
```


```
if a == 1 then {
 b = 2
 c = 3
}
 ```

becomes

```
if ($a == 1) {
    $b = 2;
    $c = 3;
}
```

## Function calls

```
a.foo()
```

becomes

```
$a->foo()
```

```
Blocklist.foo()
```

becomes

```
Blocklist::foo()
```

## Comments

```
# Comments like this are just for salty and don't get translated to php.
// Comments like this are passed as php comments
```

Comments currently only work on their own line.

## Backticks

Any code between backticks is passed verbatim. This code can be across multiple lines.

    'foo' ++ `'bar' . 'baz'`

becomes:

    "foo" . 'bar' . 'baz';

## Higher-order functions

```
foo.each(\x -> print(x))
foo.map(\x -> x + 1)
foo.any(\x -> x.isEven())
foo.all(\x -> x.isEven())
foo.select(\x -> x.isEven())
```

become

```
// each
foreach ($foo as $x) {
    print($x);
}

// map
$result = [];
foreach ($foo as $x) {
    $result []= $x + 1;
}

// any
$result = false;
foreach ($foo as $x) {
    if($x->isEven()) {
        $result = true;
        break;
    }
}

// all
$result = true;
foreach ($foo as $x) {
    if(!$x->isEven()) {
        $result = false;
        break;
    }
}

// select
$result = [];
foreach ($foo as $x) {
    if($x->isEven()) {
        $result []= x;
    }
}
```

Implicit return works correctly with these too.

```
bar := foo.map(\x -> x + 1)
```

becomes

```
public function bar() {
    $result = [];
    foreach ($foo as $x) {
        $result []= $x + 1;
    }
    return $result;
}
```

Multi-line:

```
myVar = foo.map(\x -> {
    x + 1
    y + 2
    hello("hi")
})
```

becomes

```
$myVar = [];
foreach ($foo as $x) {
    $x + 1;
    $y + 2;
    $myVar []= hello("hi");
}
```

## String Concatenation

`foo ++ bar` becomes `$foo . $bar`

## Array functions

- `"value" in array` becomes `in_array("value", $array)`
- `"key" keyin array` becomes `array_key_exists("key", $array)`
- `arr1 <> arr2` becomes `array_merge(arr1, arr2)`
- `arr1 <-> arr2` becomes `array_diff(arr1, arr2)`
- `foo[1:4]` becomes `array_slice($foo, 1, 3)`
- `foo[1:]` becomes `array_slice($foo, 1)`

## InstanceOf

- `foo instanceof Class` becomes `$foo instanceof Class`
- `foo isa Class` becomes `$foo instanceof Class`

## Multi-assign

```
foo, bar, @baz = []
```

becomes

```
$foo = [];
$bar = [];
$this->baz = [];
```

## Ranges

    (0..10).each(\x -> x + 1)

becomes

    for ($x = 0; $x <= 10; $x++) {
        $x + 1;
    }

and

    foo in 1..10

becomes

    $foo >= 1 && $foo <= 10

## Optionals

    foo?.bar
    foo?.bar()
    myVar = foo?
    myVar = foo?.bar
    myVar = foo?.bar()
    myVar = :foo?.bar

becomes

    if (!is_null($foo)) {
        $foo->bar;
    }
    if (!is_null($foo)) {
        $foo->bar();
    }
    if (!is_null($foo)) {
        $myVar = $foo;
    }
    if (!is_null($foo)) {
        $myVar = $foo->bar;
    }
    if (!is_null($foo)) {
        $myVar = $foo->bar();
    }
    if (!is_null($foo)) {
        $myVar = $foo["bar"];
    }

Chaining doesn't work right now, so you can't do `foo?.bar?.baz`

`foo.bar?` doesn't work either.

## Ternary
I don't like the ternary operator so don't support it.

The closest thing is writing if-then-else on one line, like this:

    func := if var == 0 then 0 else 1

which becomes:

    function func() {
        if ($var == 0) {
            return 0;
        } else {
            return 1;
        }
    }

The one way to generate a ternary statement is by assigning an if-then-else (must include the else clause) to a variable:

    var2 = if var == 0 then 0 else 1

generates:

    $var2 = $var == 0 ? 0 : 1;
