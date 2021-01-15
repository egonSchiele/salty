# salty

## Variables

- `foo` becomes `$foo`
- `@foo` becomes `$this->foo`
- `@@foo` becomes `static::$foo`

## Consts

```
FOO = 1
_FOO = 1
@@FOO = 1
```

becomes

```
public const FOO = 1;
private const FOO = 1;
static::$FOO = 1;
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
foo :: string
foo a := a
```

becomes

```
/**
 * @param string
 */
public function foo(string $a) {
    return $a;
}
```

```
foo :: ?string
foo a := a
```

becomes

```
/**
 * @param string|null
 */
public function foo(?string $a = null) {
    return $a;
}
```

```
foo :: ?string -> int
foo a b := a
```

becomes

```
/**
 * @param string|null
 * @param int
 */
public function foo(?string $a = null, int $b) {
    return $a;
}
```


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
foo => 1,
bar => 2,
baz => "hi"
]
```

the trailing comma is currently **required**.

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

## Feature flags

- `~foo.bar` becomes `Feature::isEnabled('foo.bar')`

## Comments

```
# Comments like this are just for salty and don't get translated to php.
// Comments like this are passed as php comments

Any code between three backticks is passed verbatim. This code can be across multiple lines.
```

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
