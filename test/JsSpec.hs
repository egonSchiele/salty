{-# LANGUAGE QuasiQuotes #-}
module JsSpec where
import Text.RawString.QQ


import Test.HUnit
import Parser
import Types
import ToJs

-- so it prints multi-line strings on multiple lines for gods sake
-- assertEqual_ expected actual = assertBool (expected == actual) failureMsg
--   where failureMsg = "expected:\n" ++ expected ++"\nbut got actual:\n" ++ actual ++ "\n"

matches str1 str2 = TestCase $ assertEqual "" str2 (saltyToJs 0 str1)

saltyBlob = [r|
  isSafe str lists := lists.any(l -> l.isBlocked(str))

  isBlocked str := do
    return @list.any(term -> strpos(term, str) !== false)
    end

  isUnsafe str := !isSafe(str)
  |]

phpBlob = [r|
  function isSafe(str, lists) {
    result = false;
    foreach (lists as l) {
      if (l.isBlocked(str)) {
        result = true;
        break;
      }
    }
    return result;
  }

  function isBlocked(str) {
    result = false;
    foreach (list as term) {
      if (strpos(term, str) !== false) {
        result = true;
        break;
      }
    }
    return result;
  }

  function isUnsafe(str) {
    return !isSafe(str);
  }
  |]

longerTest = saltyBlob `matches` phpBlob

multiLineEach = [r|foo.each(\x -> {
    x + 1
    y + 2
    hello("hi")
  })
|]

multiLineEachResult = [r|foo.forEach((x) => {
  x + 1;
  y + 2;
  hello("hi");
});|]

multiLineEachTest = multiLineEach `matches` multiLineEachResult

multiLineMap = [r|foo.map(\x -> {
    x + 1
    y + 2
    hello("hi")
  })
|]

multiLineMapResult = [r|foo.map((x) => {
  x + 1;
  y + 2;
  return hello("hi");
});|]

multiLineMapTest = multiLineMap `matches` multiLineMapResult

multiLineMapAssign = [r|myVar = foo.map(\x -> {
    x + 1
    y + 2
    hello("hi")
  })
|]

multiLineMapAssignResult = [r|myVar = foo.map((x) => {
  x + 1;
  y + 2;
  return hello("hi");
});|]

multiLineMapAssignTest = multiLineMapAssign `matches` multiLineMapAssignResult

multiLineSelect = [r|foo.select(\x -> {
    x + 1
    y + 2
    hello("hi")
  })
|]

multiLineSelectResult = [r|foo.filter((x) => {
  x + 1;
  y + 2;
  return hello("hi");
});|]

multiLineSelectTest = multiLineSelect `matches` multiLineSelectResult

multiLineAny = [r|foo.any(\x -> {
    x + 1
    y + 2
    hello("hi")
  })
|]

multiLineAnyResult = [r|foo.some((x) => {
  x + 1;
  y + 2;
  return hello("hi");
});|]

multiLineAnyTest = multiLineAny `matches` multiLineAnyResult

multiLineAll = [r|foo.all(\x -> {
    x + 1
    y + 2
    hello("hi")
  })
|]

multiLineAllResult = [r|foo.every((x) => {
  x + 1;
  y + 2;
  return hello("hi");
});|]

multiLineAllTest = multiLineAll `matches` multiLineAllResult

multiLineArrays = [r|
class Foo implements Bar where

foo := {
    [
        ["", "en", "US", "USD", false, false, false],
        ["", "en", "US", "USD", false, false, false]
    ]
}
|]

multiLineArraysTest = [r|class Foo implements Bar {
  foo() {
    return [
      ["", "en", "US", "USD", false, false, false],
      ["", "en", "US", "USD", false, false, false],
    ];
  }
}|]

guardTest = [r|foo := guard
  | x == 1 -> x.foo()
  | y == 2 -> x.map(\y -> y + 1)
|]

guardResult = [r|const foo = () => {
  if (x === 1) {
    return x.foo();
  } elseif (y === 2) {
    return x.map((y) => y + 1);
  }
}|]

guardTestWithArgs = [r|foo a bar := guard
  | x == 1 -> x.foo()
  | otherwise -> (new Foo()).bar()
|]

guardResultWithArgs = [r|const foo = (a, bar) => {
  if (x === 1) {
    return x.foo();
  } else {
    return (new Foo()).bar();
  }
}|]

guardTestWithWhere = [r|foo a bar := guard
  | awesome -> x.foo()
  | otherwise -> x.map(\y -> y + 1)
  where awesome = 1 + 1
  and blossom = Foo.new()
|]

guardResultWithWhere = [r|const foo = (a, bar) => {
  awesome = 1 + 1;
  blossom = <Foo />
  if (awesome) {
    return x.foo();
  } else {
    return x.map((y) => y + 1);
  }
}|]

guardTestComplex = [r|foo :: bool
foo := guard
  | (bar in ["aa", "bb"]) && (baz in ["cc", "dd"]) -> true
  | bar == "fr" && baz == "fr" -> true
  | bar == "de" && baz == "de" -> true
  | otherwise -> false
|]

guardTestComplexResult = [r|const foo = () => {
  if ((["aa", "bb"].includes(bar)) && (["cc", "dd"].includes(baz))) {
    return true;
  } elseif (bar === "fr" && baz === "fr") {
    return true;
  } elseif (bar === "de" && baz === "de") {
    return true;
  } else {
    return false;
  }
}|]

guardTestAsSwitch = [r|todos state action := guard(hi)
  | "ADD" -> todo(null, action)
  | "TOGGLE" -> state.map(\t -> todo(t, action))
  | otherwise -> state
|]

guardTestAsSwitchResult = [r|const todos = (state, action) => {
  switch (hi) {
    case "ADD":
      return todo(null, action);
    case "TOGGLE":
      return state.map((t) => todo(t, action));
    default:
      return state;
  }
}|]

longClass = [r|constructor props := {
  super(props)
  this.state = {
    activeItem: (window.localStorage.getItem('step') || 0)
  };
  React.useEffect((\_ -> window.localStorage.setItem('step', @@activeItem)))
}|]

longClassResult = [r|const constructor = (props) => {
  super(props);
  this.state = {
    "activeItem": (window.localStorage.getItem("step") || 0)
  }
  return React.useEffect((() => {
    return window.localStorage.setItem("step", this.state.activeItem);
  }));
}|]

doFunc = [r|constructor props := {
  React.useEffect() do \_ ->
    window.localStorage.setItem('step', @@activeItem)
  end
}|]

doFuncResult = [r|const constructor = (props) => {
  return React.useEffect(() => {
    return window.localStorage.setItem("step", this.state.activeItem);
  });
}|]

jsTests = [
    multiLineEachTest,
    multiLineMapTest,
    multiLineMapAssignTest,
    multiLineSelectTest,
    multiLineAnyTest,
    multiLineAllTest,
    multiLineArrays `matches` multiLineArraysTest,
    guardTest `matches` guardResult,
    guardTestWithArgs `matches` guardResultWithArgs,
    guardTestWithWhere `matches` guardResultWithWhere,
    guardTestComplex `matches` guardTestComplexResult,
    guardTestAsSwitch `matches` guardTestAsSwitchResult,
    longClass `matches` longClassResult,
    doFunc `matches` doFuncResult,
    -- operations
    "foo = 1" `matches` "foo = 1;",
    "bar = 'adit'" `matches` "bar = \"adit\";",
    "@foo = 1" `matches` "this.foo = 1;",
    "@@foo = 1" `matches` "this.setState({\n    foo: 1\n});",

    "a = a - 1" `matches` "a = a - 1;",
    "a -= 1" `matches` "a = a - 1;",
    "a == 1" `matches` "a === 1;",
    "5 * 5" `matches` "5 * 5;",
    "x % 2" `matches` "x % 2;",
    "x % 2 == 0" `matches` "x % 2 === 0;",
    "foo + bar" `matches` "foo + bar;",
    "'foo' + 'bar'" `matches` "\"foo\" + \"bar\";",
    "a + b + c" `matches` "a + b + c;",

    "(a + 1) * 20" `matches` "(a + 1) * 20;",
    "(a + 1) * (b - 10)" `matches` "(a + 1) * (b - 10);",
    "(a + (b * (c / 30)))" `matches` "(a + (b * (c / 30)));",
    "foo() + a.bar()" `matches` "foo() + a.bar();",
    "foo []= bar" `matches` "foo.push(bar);",
    "foo ++ bar" `matches` "foo + bar;",
    "foo <> bar" `matches` "Object.assign(foo, bar);",
    "foo <-> bar" `matches` "array_diff(foo, bar);",
    "foo in bar" `matches` "bar.includes(foo);",
    "'foo' in bar" `matches` "bar.includes(\"foo\");",
    "foo keyin bar" `matches` "bar.hasOwnProperty(foo);",
    "foo <=> bar" `matches` "foo <=> bar;",
    "foo = hello_there(hi(2) <> 1)" `matches` "foo = hello_there(Object.assign(hi(2), 1));",
    "foo = hello_there(hi(2) + 1)" `matches` "foo = hello_there(hi(2) + 1);",
    "foo, bar, baz = []" `matches` "foo = [];\nbar = [];\nbaz = [];",
    "key = @@KEY ++ preg_replace('/[^a-z\\d_]/i', '_', str).join(\"_\") ++ last" `matches` "key = KEY + preg_replace(\"/[^a-z\\d_]/i\", \"_\", str).join(\"_\") + last;",

    -- function definitions
    "build a b := return 2" `matches` "const build = (a, b) => {\n  return 2;\n}",
    "@@build a b := return 2" `matches` "const build = (a, b) => {\n  return 2;\n}",
    "incr a := return a + 1" `matches` "const incr = (a) => {\n  return a + 1;\n}",
    "foo := a.foo()" `matches` "const foo = () => {\n  return a.foo();\n}",
    "foo a b := a + 1 + b + 2" `matches` "const foo = (a, b) => {\n  return a + 1 + b + 2;\n}",
    "foo a b := (a + 1) + (b - 2)" `matches` "const foo = (a, b) => {\n  return (a + 1) + (b - 2);\n}",
    "foo a b := { a + b }" `matches` "const foo = (a, b) => {\n  return a + b;\n}",
    -- no visibility (public/private) for functions in global scope:
    "_foo a := @a = a" `matches` "const foo = (a) => {\n  this.a = a;\n}",
    "@@_foo a := @a = a" `matches` "const foo = (a) => {\n  this.a = a;\n}",
    -- but do add visibility in class scope:
    "class Foo {\n_foo a := @a = a\n}" `matches` "class Foo {\n  foo(a) {\n    this.a = a;\n  }\n}",
    "class Foo {\n@@_foo a := @a = a\n}" `matches` "class Foo {\n  foo(a) {\n    this.a = a;\n  }\n}",
    "foo2 := 2 + 2" `matches` "const foo2 = () => {\n  return 2 + 2;\n}",
    "__construct a := @a = a" `matches` "const __construct = (a) => {\n  this.a = a;\n}",
    "foo ...a := a" `matches` "const foo = (...a) => {\n  return a;\n}",

    -- pass by reference
    "incr &count := ++count" `matches` "const incr = (count) => {\n  count = count + 1;\n}",
    "foo a b := bar\n  where baz = b + 1\n  and bar = a + baz" `matches` "const foo = (a, b) => {\n  baz = b + 1;\n  bar = a + baz;\n  return bar;\n}",

    -- fails, variables don't have the concept of pass by reference or not yet
    -- "foo = &bar" `matches` "foo = &bar",

    -- plus plus
    "++bar" `matches` "bar = bar + 1;",
    "bar++" `matches` "bar = bar + 1;",
    "--bar" `matches` "bar = bar - 1;",
    "bar--" `matches` "bar = bar - 1;",

    -- parens tests
    "(a + b)" `matches` "(a + b);",
    "((a + b))" `matches` "((a + b));",
    "(((a + b)))" `matches` "(((a + b)));",
    "(((foo())))" `matches` "(((foo())));",
    "(((a.foo())))" `matches` "(((a.foo())));",
    "((a + 1) * (b - 4))" `matches` "((a + 1) * (b - 4));",
    "(foo).bar" `matches` "(foo).bar;",
    "(foo).bar()" `matches` "(foo).bar();",
    "(new Foo()).bar()" `matches` "(new Foo()).bar();",

    "var = (new Foo(:hash.key)).someFunc()" `matches` "var = (new Foo(hash[\"key\"])).someFunc();",

    -- braces tests
    "fib x := {\nif x < 2 then {\nreturn x\n} else {\nreturn fib(x - 1) + fib(x - 2)\n}\n}" `matches` "const fib = (x) => {\n  if (x < 2) {\n    return x;\n  } else {\n    return fib(x - 1) + fib(x - 2);\n  }\n}",
    "fib x := {\na + b\n b + c\n }\n \n foo a b := a + b" `matches` "const fib = (x) => {\n  a + b;\n  return b + c;\n}\nconst foo = (a, b) => {\n  return a + b;\n}",
    "fib x := {\na + b\n b + c\n }\n \n foo a b := { a + b }" `matches` "const fib = (x) => {\n  a + b;\n  return b + c;\n}\nconst foo = (a, b) => {\n  return a + b;\n}",

    -- hash tests
    "argv[1]" `matches` "argv[1];",
    "argv[1][2]" `matches` "argv[1][2];",
    "fib(argv[1])" `matches` "fib(argv[1]);",
    "var_dump(fib(argv[1]))" `matches` "var_dump(fib(argv[1]));",

    -- hash dot notation tests
    ":foo.bar.baz" `matches` "foo[\"bar\"][\"baz\"];",
    ":foo.bar.baz.1" `matches` "foo[\"bar\"][\"baz\"][1];",
    ":argv.1.2" `matches` "argv[1][2];",
    ":@foo.bar" `matches` "this.foo[\"bar\"];",
    ":@@foo.bar" `matches` "this.state.foo[\"bar\"];",
    ":bar.baz ?? 1" `matches` "bar[\"baz\"] ?? 1;",

    -- if statement
    "if a = 1 then {\n b = 2\n c = 3\n }" `matches`"if (a = 1) {\n  b = 2;\n  c = 3;\n}",
    "if a != 'foo' then return 2 else return 3" `matches` "if (a !== \"foo\") {\n  return 2;\n} else {\n  return 3;\n}",
    "foo := if x % 2 == 0 then 'even' else 'odd'" `matches` "const foo = () => {\n  if (x % 2 === 0) {\n    return \"even\";\n  } else {\n    return \"odd\";\n  }\n}",

    -- arity for else
    "fib x := if x < 2 then x else fib(x - 1) + fib(x - 2)" `matches` "const fib = (x) => {\n  if (x < 2) {\n    return x;\n  } else {\n    return fib(x - 1) + fib(x - 2);\n  }\n}",

    -- while statement
    "while foo == 1 {\nfoo = 2\n}" `matches`"while (foo === 1) {\n  foo = 2;\n}",
    "while foo == 1 {\nfoo = 2\nbar = 3\n}" `matches`"while (foo === 1) {\n  foo = 2;\n  bar = 3;\n}",

    -- function calls
    "Blocklist.foo()" `matches` "Blocklist.foo();",
    "a.foo()" `matches` "a.foo();",
    "@a.foo()" `matches` "this.a.foo();",
    "@foo()" `matches` "this.foo();",
    "@@foo()" `matches` "this.state.foo();",

    "Blocklist.foo(1, 2)" `matches` "Blocklist.foo(1, 2);",
    "a.foo(1, 2)" `matches` "a.foo(1, 2);",
    "@a.foo(1, 2)" `matches` "this.a.foo(1, 2);",
    "@foo(1, 2)" `matches` "this.foo(1, 2);",
    "@@foo(1, 2)" `matches` "this.state.foo(1, 2);",

    "Blocklist.foo(b)" `matches` "Blocklist.foo(b);",
    "a.foo(b)" `matches` "a.foo(b);",
    "@a.foo(b)" `matches` "this.a.foo(b);",
    "@foo(b)" `matches` "this.foo(b);",
    "@@foo(b)" `matches` "this.state.foo(b);",

    "Blocklist.foo(b.bar())" `matches` "Blocklist.foo(b.bar());",
    "a.foo(b.bar())" `matches` "a.foo(b.bar());",
    "@a.foo(@bar())" `matches` "this.a.foo(this.bar());",
    "@foo(@@bar())" `matches` "this.foo(this.state.bar());",
    "@@foo(@b.bar())" `matches` "this.state.foo(this.b.bar());",
    "a.foo().bar().baz().func().func2()" `matches` "a.foo().bar().baz().func().func2();",
    "foo . bar . baz $ myVar" `matches` "foo(bar(baz(myVar)));",

    -- attr access
    "foo.bar" `matches` "foo.bar;",
    "@foo.bar" `matches` "this.foo.bar;",
    "@@foo.bar" `matches` "this.state.foo.bar;",
    "Blocklist.foo" `matches` "Blocklist.foo;",
    "Blocklist.FOO" `matches` "Blocklist.FOO;",
    "foo.bar = 1" `matches` "foo.bar = 1;",
    "foo.bar = 'hello'" `matches` "foo.bar = \"hello\";",
    "foo.bar = 2 + 2" `matches` "foo.bar = 2 + 2;",

    -- negate
    "!foo" `matches` "!foo;",

    -- class definition
    "class Blocklist {\n @foo := p(\"hi!\")\n }" `matches` "class Blocklist {\n  foo() {\n    return console.log(\"hi!\");\n  }\n}",
    "class Blocklist extends Bar {\n @foo := p(\"hi!\")\n }" `matches` "class Blocklist extends Bar {\n  foo() {\n    return console.log(\"hi!\");\n  }\n}",
    "class Blocklist implements Bar {\n @foo := p(\"hi!\")\n }" `matches` "class Blocklist implements Bar {\n  foo() {\n    return console.log(\"hi!\");\n  }\n}",
    "class Blocklist extends Foo implements Bar {\n @foo := p(\"hi!\")\n }" `matches` "class Blocklist extends Foo implements Bar {\n  foo() {\n    return console.log(\"hi!\");\n  }\n}",

    -- class creation with where
    "class Foo implements Bar where\n\nattr1 = 'hi'\n__construct name := @attr1 = name" `matches` "class Foo implements Bar {\n  attr1 = \"hi\";\n  __construct(name) {\n    this.attr1 = name;\n  }\n}",

    -- object creation
    "class Blocklist {\n@foo := p(\"hi!\")\n }\n b = new Blocklist()\n b.foo()" `matches` "class Blocklist {\n  foo() {\n    return console.log(\"hi!\");\n  }\n}\nb = new Blocklist();\nb.foo();",

    -- function type signature
    -- "foo :: string\nfoo := 'hello'" `matches` "\n/**\n * @return string\n */\nfunction foo() {\n  return \"hello\";\n}",
    -- "foo :: string -> string\nfoo a := a" `matches` "\n/**\n * @param string\n * @return string\n */\nfunction foo(string a) {\n  return a;\n}",
    -- "foo :: string? -> string?\nfoo a := a" `matches` "\n/**\n * @param string|null\n * @return string|null\n */\nfunction foo(?string a = null) {\n  return a;\n}",
    -- "foo :: string? -> int -> string?\nfoo a b := a" `matches` "\n/**\n * @param string|null\n * @param int\n * @return string|null\n */\nfunction foo(?string a = null, int b) {\n  return a;\n}",
    -- var type signature
    -- "var :: string" `matches` "\n/** @var string */",
    -- "var :: string?" `matches` "\n/** @var string|null */",
    -- null, true, false
    "a = true" `matches` "a = true;",
    "b = false" `matches` "b = false;",
    "c = null" `matches` "c = null;",
    "_SAMPLE_RATE = 0.001" `matches` "_SAMPLE_RATE = 0.001;",
    "const _SAMPLE_RATE = 0.001" `matches` "const _SAMPLE_RATE = 0.001;",
    "foo = ONE + TWO" `matches` "foo = ONE + TWO;",
    "class Foo {\n_SAMPLE_RATE = 0.001\n}" `matches` "class Foo {\n  _SAMPLE_RATE = 0.001;\n}",
    "class Foo {\nSAMPLE_RATE = 0.001\n}" `matches` "class Foo {\n  SAMPLE_RATE = 0.001;\n}",
    "class Foo {\n@@SAMPLE_RATE = 0.001\n}" `matches` "class Foo {\n  SAMPLE_RATE = 0.001;\n}",
    "class Foo {\n@@_SAMPLE_RATE = 0.001\n}" `matches` "class Foo {\n  _SAMPLE_RATE = 0.001;\n}",
    "class Foo {\nconst _SAMPLE_RATE = 0.001\n}" `matches` "class Foo {\n  const _SAMPLE_RATE = 0.001;\n}",

    -- return statements
    "return foo" `matches` "return foo;",
    "return (1 + 2)" `matches` "return (1 + 2);",

    -- implicit returns
    "foo := 5" `matches` "const foo = () => {\n  return 5;\n}",
    "foo := bar = 5" `matches` "const foo = () => {\n  bar = 5;\n}",
    "foo := bar []= 5" `matches` "const foo = () => {\n  bar.push(5);\n  return bar;\n}",
    "foo := \"hello\"" `matches` "const foo = () => {\n  return \"hello\";\n}",
    "foo x := if x then 'hi' else 'hello'" `matches` "const foo = (x) => {\n  if (x) {\n    return \"hi\";\n  } else {\n    return \"hello\";\n  }\n}",
    "fib x := if x < 2 then x" `matches` "const fib = (x) => {\n  if (x < 2) {\n    return x;\n  }\n}",
    "foo := bar ||= 1" `matches` "const foo = () => {\n  bar = bar ?? 1;\n  return bar;\n}",

    -- higher order functions
    "arr.any(\\x -> x == 1)" `matches` "arr.some((x) => x === 1);",
    "arr.all(\\x -> x.isEven())" `matches` "arr.every((x) => x.isEven());",
    "arr.select(\\x -> x.isEven())" `matches` "arr.filter((x) => x.isEven());",
    "arr.each(\\x -> print(x))" `matches` "arr.forEach((x) => {\n  print(x);\n});",
    "@adit.map(\\x -> x + 1)" `matches` "this.adit.map((x) => x + 1);",
    "10.times(p('hello'))" `matches` "for (x = 1; x <= 10; x++) {\n  console.log(\"hello\");\n}",
    "10.times(\\i -> new Hello(i))" `matches` "for (i = 1; i <= 10; i++) {\n  new Hello(i);\n}",
    -- same but w parens
    "(@adit).map(\\x -> x + 1)" `matches` "(this.adit).map((x) => x + 1);",
    "users = shops.map(\\s -> s.user)" `matches` "users = shops.map((s) => s.user);",
    "users = shops.map(\\s x -> s.user)" `matches` "users = shops.map((s, x) => s.user);",
    "shops.each(\\s x -> s.user)" `matches` "shops.forEach((s, x) => {\n  s.user;\n});",
    "@shops().each(\\s -> s.user)" `matches` "this.shops().forEach((s) => {\n  s.user;\n});",
    "[foo, bar, baz].map(\\shop -> shop.listings)" `matches` "[foo, bar, baz].map((shop) => shop.listings);",
    "foo[:5].map(\\shop -> shop.listings)" `matches` "foo.slice(0, 6).map((shop) => shop.listings);",
    "@shops().find(1).each(\\s -> s.user)" `matches` "this.shops().find(1).forEach((s) => {\n  s.user;\n});",
    "Foo.shops(arg1).each(\\s -> s.user)" `matches` "Foo.shops(arg1).forEach((s) => {\n  s.user;\n});",
    "@@shops().map(\\s -> s.user)" `matches` "this.state.shops().map((s) => s.user);",
    "Foo.shops().any(\\s -> s.user)" `matches` "Foo.shops().some((s) => s.user);",
    "@shops().all(\\s -> s.user)" `matches` "this.shops().every((s) => s.user);",
    "@shops().select(\\s -> s.user)" `matches` "this.shops().filter((s) => s.user);",

    "@state.todos.map(\\todo -> @makeTodo(todo))" `matches` "this.state.todos.map((todo) => this.makeTodo(todo));",

    -- chain a function onto the end of a HoF
    "count_alive = shops.select(\\s -> s.isAlive()).count()" `matches` "count_alive = shops.filter((s) => s.isAlive()).count();",
    -- TODO attr access and array slices don't work on this
    "count_alive = shops.select(\\s -> s.isAlive()).foo" `matches` "count_alive = shops.filter((s) => s.isAlive()).foo;",
    -- "count_alive = shops.select(\\s -> s.isAlive())[:2]" `matches` "",
    -- TODO can't chain more than one function yet
    -- "count_alive = shops.select(\\s -> s.isAlive()).uniq().count()" `matches` "",

    -- assigning accVar manually
    -- nothing gets assigned in an each
    "myAcc = foo.each(\\x -> print(x))" `matches` "foo.forEach((x) => {\n  print(x);\n});",
    "@myAcc = foo.map(\\x -> x + 1)" `matches` "this.myAcc = foo.map((x) => x + 1);",
    "@@myAcc = foo.any(\\x -> x.isEven())" `matches` "this.setState({\n    myAcc: foo.some((x) => x.isEven())\n});",
    "myAcc = foo.all(\\x -> x.isEven())" `matches` "myAcc = foo.every((x) => x.isEven());",

    -- assigning accVar manually plus implicit return
    "bar := myAcc = foo.each(\\x -> print(x))" `matches` "const bar = () => {\n  foo.forEach((x) => {\n    print(x);\n  });\n}",
    "bar := @myAcc = foo.map(\\x -> x + 1)" `matches` "const bar = () => {\n  this.myAcc = foo.map((x) => x + 1);\n  return this.myAcc;\n}",
    "bar := @@myAcc = foo.any(\\x -> x.isEven())" `matches` "const bar = () => {\n  this.setState({\n      myAcc: foo.some((x) => x.isEven())\n  });\n}",
    "bar := myAcc = foo.all(\\x -> x.isEven())" `matches` "const bar = () => {\n  myAcc = foo.every((x) => x.isEven());\n  return myAcc;\n}",

    -- implicit returns for higher order functions
    "bar := foo.each(\\x -> print(x))" `matches` "const bar = () => {\n  foo.forEach((x) => {\n    print(x);\n  });\n}",
    "bar := foo.map(\\x -> x + 1)" `matches` "const bar = () => {\n  return foo.map((x) => x + 1);\n}",
    "bar := foo.any(\\x -> x.isEven())" `matches` "const bar = () => {\n  return foo.some((x) => x.isEven());\n}",
    "bar := foo.all(\\x -> x.isEven())" `matches` "const bar = () => {\n  return foo.every((x) => x.isEven());\n}",
    "bar := foo.select(\\x -> x.isEven())" `matches` "const bar = () => {\n  return foo.filter((x) => x.isEven());\n}",

    -- keywords
    "use Foo" `matches` "use Foo;",
    "use Foo\\Bar" `matches` "use Foo\\Bar;",
    "use Foo\\Bar as Baz" `matches` "use Foo\\Bar as Baz;",
    "throw new Exception()" `matches` "throw new Exception();",
    "throw new Exception('foo')" `matches` "throw new Exception(\"foo\");",
    "throw e" `matches` "throw e;",
    "require 'foo.php'" `matches` "require \"foo.php\";",
    "require_once 'foo.php'" `matches` "require_once \"foo.php\";",
    "namespace Foo" `matches` "namespace Foo;",
    "namespace Foo\\Bar" `matches` "namespace Foo\\Bar;",
    "echo 'hi'" `matches` "echo \"hi\";",
    "greet := echo 'hi'" `matches` "const greet = () => {\n  echo \"hi\";\n}",
    "break" `matches` "break;",

    -- arrays
    "foo = [1, 2, 3]" `matches` "foo = [1, 2, 3];",
    "a = {\n foo: 1,\n bar: 2,\n cat: 'hello',\n }" `matches` "a = {\n  \"foo\": 1,\n  \"bar\": 2,\n  \"cat\": \"hello\"\n}",
    "a = {\n foo: 1,\n bar: 2,\n cat: 'hello'\n }" `matches` "a = {\n  \"foo\": 1,\n  \"bar\": 2,\n  \"cat\": \"hello\"\n}",

    -- array slices
    "foo[1:]" `matches` "foo.slice(1);",
    "foo[:2]" `matches` "foo.slice(0, 3);",
    "foo[:]" `matches` "foo.slice(0);",
    "foo[1:5]" `matches` "foo.slice(1, 6);",
    "foo[start:]" `matches` "foo.slice(start);",
    "foo[2:count(foo)]" `matches` "foo.slice(2, count(foo)-1);",

    -- array splices
    "foo[1:] = arr" `matches` "foo.splice(1, foo.length, arr);",
    "foo[:2] = arr" `matches` "foo.splice(0, 2, arr);",
    "foo[:] = arr" `matches` "foo.splice(0, foo.length, arr);",
    "foo[1:5] = arr" `matches` "foo.splice(1, 4, arr);",
    "foo[start:] = arr" `matches` "foo.splice(start, foo.length, arr);",
    "foo[2:count(foo)] = arr" `matches` "foo.splice(2, count(foo) - 2, arr);",

    -- string slices
    "foo<1>" `matches` "foo.charAt(1);",
    "foo<1:>" `matches` "foo.substring(1);",
    "foo<:2>" `matches` "foo.substring(0, 3);",
    "foo<:>" `matches` "foo.substring(0);",
    "foo<1:5>" `matches` "foo.substring(1, 6);",
    "foo<start:>" `matches` "foo.substring(start);",
    "foo<2:count(foo)>" `matches` "foo.substring(2, count(foo)-1);",
    -- instanceof
    "foo instanceof Class" `matches` "foo instanceof Class;",
    "foo isa Class" `matches` "foo instanceof Class;",

    -- string interpolation
    "oranges = \"I have {number} oranges to eat.\"" `matches` "oranges = \"I have {number} oranges to eat.\";",
    "foo = \"hello there\"" `matches` "foo = \"hello there\";",

    -- ranges
    "1..10" `matches` "[1,2,3,4,5,6,7,8,9,10];",
    -- "1..foo.bar" `matches` "a range: (1..foo.bar)",
    -- "start..end" `matches` "a range: (start..end)",

    -- TODO failing spec
    -- "take(5, (1..10))" `matches` "take(5, [1,2,3,4,5,6,7,8,9,10]);",
    "take(5, 1..10)" `matches` "take(5, [1,2,3,4,5,6,7,8,9,10]);",

    -- classic for loop using each + range
    "(1..10).each(\\x -> x + 1)" `matches` "for (x = 1; x <= 10; x++) {\n  x + 1;\n}",
    -- "(start..end).each(\\x -> x + 1)" `matches` "for (x = start; x <= end; x++) {\n  x + 1;\n}",
    "foo in 1..10" `matches` "foo >= 1 && foo <= 10;",
    "foo in (1..10)" `matches` "foo >= 1 && foo <= 10;",
    -- "foo in (start..end.baz)" `matches` "foo >= start && foo <= end.baz;",
    -- "if foo in (start..end.baz) then {\nhi()\n}" `matches` "if (foo >= start && foo <= end.baz) {\n  hi();\n}",

    -- scope
    "class Foo {\nbar = 1\n}" `matches` "class Foo {\n  bar = 1;\n}",
    "class Foo {\n_bar = 1\n}" `matches` "class Foo {\n  _bar = 1;\n}",
    "class Foo {\n@@_bar = 1\n}" `matches` "class Foo {\n  this.setState({\n      _bar: 1\n  });\n}",
    "class Foo {\n@@bar = 1\n}" `matches` "class Foo {\n  this.setState({\n      bar: 1\n  });\n}",

    -- strip semicolons
    "foo = 1;\nbar = 2" `matches` "foo = 1;\nbar = 2;",

    -- don't fail on dollar signs
    "foo = 1;\nbar = 2;" `matches` "foo = 1;\nbar = 2;",

    -- ternary
    "func := if var == 0 then 0 else 1" `matches` "const func = () => {\n  if (var === 0) {\n    return 0;\n  } else {\n    return 1;\n  }\n}",
    "var2 = if var == 0 then 0 else 1" `matches` "var2 = var === 0 ? 0 : 1;",
    "func := if var == 0 then (if var2 == 1 then 1 else 2) else 1" `matches`  "const func = () => {\n  if (var === 0) {\n    if (var2 === 1) {\n      return 1;\n    } else {\n      return 2;\n    }\n  } else {\n    return 1;\n  }\n}",
    "func := if var == 0 then 0" `matches` "const func = () => {\n  if (var === 0) {\n    return 0;\n  }\n}",
    "var2 = if var == 0 then 0" `matches` "var2 = null;\nif (var === 0) {\n  var2 = 0;\n}",

    -- backticks for php
    "'foo' ++ `'bar' . 'baz'`" `matches` "\"foo\" + 'bar' . 'baz';",

    -- hash table
    "{a: 1, [b]: 2, [@c]: 3}" `matches` "{\n  \"a\": 1,\n  b: 2,\n  this.c: 3\n}",
    "{ stemmed: stemmed, unstemmed: unstemmed }" `matches` "{\n  \"stemmed\": stemmed,\n  \"unstemmed\": unstemmed\n}",

    -- string
    "\"foo\"" `matches` "\"foo\";",
    "'foo'" `matches` "\"foo\";",
    "\"'foo'\"" `matches` "\"'foo'\";",
    "\"'foo' and 'bar'\"" `matches` "\"'foo' and 'bar'\";",
    "\"<div class='foo bar'>\"" `matches` "\"<div class='foo bar'>\";",

    -- built in functions
    "foo.uniq()" `matches` "let foo = [...new Set(foo)];",
    "foo.sub(\"foo\", 'bar')" `matches` "foo.replace(\"foo\", \"bar\");",

    -- multi-assign
    "foo, bar = baz" `matches` "foo = baz[0];\nbar = baz[1];",
    "foo, bar = null" `matches` "foo = null;\nbar = null;",
    "foo, bar = 0" `matches` "foo = 0;\nbar = 0;",
    "foo, bar = hi('.', array)" `matches` "result = hi(\".\", array);\nfoo = result[0];\nbar = result[1];",
    "foo, bar = obj.func()" `matches` "result = obj.func();\nfoo = result[0];\nbar = result[1];",
    "foo, bar = (new Foo(bar)).run()" `matches` "result = (new Foo(bar)).run();\nfoo = result[0];\nbar = result[1];",

    -- hash lookup
    "arr.1 - arr.0" `matches` "arr[1] - arr[0];",
    "func(a.1, a.2)" `matches` "func(a[1], a[2]);",
    "arr.1.count()" `matches` "arr[1].count();",

    -- hash shorthand
    "{ foo, bar, baz }" `matches` "{ foo, bar, baz }",
    "{ foo, bar, baz } = arr" `matches` "{ foo, bar, baz } = arr;",
    "{ foo }" `matches` "{ foo }",


    -- new keyword
    "new self()" `matches` "new self();",
    "new self(1, foo)" `matches` "new self(1,foo);",

    "const [name, setName] = React.useState('')" `matches` "const [name, setName] = React.useState(\"\");",
    "(window.localStorage.getItem('step') || 0)" `matches` "(window.localStorage.getItem(\"step\") || 0);",

    -- jsx tests
    "<div>hi</div>" `matches` "<div>hi</div>",
    "foo = <div>hi</div>" `matches` "foo = <div>hi</div>",
    "foo := <div>hi</div>" `matches` "const foo = () => {\n  return <div>hi</div>\n}",

    "<div className='foo' val={@myVar}>hi</div>" `matches` "<div className='foo' val={@myVar}>hi</div>",
    "foo = <div className='foo' val={@myVar}>hi</div>" `matches` "foo = <div className='foo' val={@myVar}>hi</div>",
    "foo := <div className='foo' val={@myVar}>hi</div>" `matches` "const foo = () => {\n  return <div className='foo' val={@myVar}>hi</div>\n}",

    "<div />" `matches` "<div />",
    "<div className='foo' val={@myVar} />" `matches`"<div className='foo' val={@myVar} />",
    "foo = <div className='foo' val={@myVar} />" `matches` "foo = <div className='foo' val={@myVar} />",
    "foo := <div className='foo' val={@myVar} />" `matches` "const foo = () => {\n  return <div className='foo' val={@myVar} />\n}",

    -- imports
    "import * as React from \"React\"" `matches` "import * as React from \"React\";",
    "import React from \"React\"" `matches` "import React from \"React\";",
    "import { createElement } from \"React\"" `matches` "import { createElement } from \"React\";",

    -- jsx
    "h1 'hi there'" `matches` "<h1>hi there</h1>",
    "h1 (myVar)" `matches` "<h1>{myVar}</h1>",
    "h1 (@myVar)" `matches` "<h1>{this.myVar}</h1>",
    "h1 (@@myVar)" `matches` "<h1>{this.state.myVar}</h1>",
    "h1 (myVar.foo)" `matches` "<h1>{myVar.foo}</h1>",
    "h1 (@myVar.bar)" `matches` "<h1>{this.myVar.bar}</h1>",
    "h1 (@@myVar.baz)" `matches` "<h1>{this.state.myVar.baz}</h1>",
    "h1 ('hello, ' ++ name ++ '!')" `matches` "<h1>{\"hello, \" + name + \"!\"}</h1>"
  ]
