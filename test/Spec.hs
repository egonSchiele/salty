{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ


import Test.HUnit
import Parser
import Types
import ToPhp

-- so it prints multi-line strings on multiple lines for gods sake
-- assertEqual_ expected actual = assertBool (expected == actual) failureMsg
--   where failureMsg = "expected:\n" ++ expected ++"\nbut got actual:\n" ++ actual ++ "\n"

matches str1 str2 = TestCase $ assertEqual "" (str2 ++ "\n") (saltyToPhp str1)

makeToPhpTest :: (Salty, String) -> Test
makeToPhpTest (salty,expectedStr) = (toPhp salty) `matches` expectedStr

saltyBlob = [r|
  isSafe str lists := lists.any(\l -> l.isBlocked(str))

  isBlocked str := do
    return @list.any(\term -> strpos(term, str) !== false)
    end

  isUnsafe str := !isSafe(str)
  |]

phpBlob = [r|
  public function isSafe($str, $lists) {
    $result = false;
    foreach ($lists as $l) {
      if ($l->isBlocked($str)) {
        $result = true;
        break;
      }
    }
    return $result;
  }

  public function isBlocked($str) {
    $result = false;
    foreach ($list as $term) {
      if (strpos(term, str) !== false) {
        $result = true;
        break;
      }
    }
    return $result;
  }

  public function isUnsafe($str) {
    return !isSafe($str);
  }
  |]

longerTest = saltyBlob `matches` phpBlob

transpileTests = [
    -- operations
    "foo = 1" `matches` "$foo = 1;",
    "bar = 'adit'" `matches` "$bar = \"adit\";",
    "@foo = 1" `matches` "$this->foo = 1;",
    "@@foo = 1" `matches` "static::$foo = 1;",

    "a = a - 1" `matches` "$a = $a - 1;",
    "a -= 1" `matches` "$a = $a - 1;",
    "a == 1" `matches` "$a == 1;",
    "5 * 5" `matches` "5 * 5;",
    "foo + bar" `matches` "$foo + $bar;",
    "'foo' + 'bar'" `matches` "\"foo\" + \"bar\";",
    "a + b + c" `matches` "$a + $b + $c;",

    "(a + 1) * 20" `matches` "($a + 1) * 20;",
    "(a + 1) * (b - 10)" `matches` "($a + 1) * ($b - 10);",
    "(a + (b * (c / 30)))" `matches` "($a + ($b * ($c / 30)));",
    "foo() + a.bar()" `matches` "foo() + $a->bar();",

    -- public function definitions
    "build a b := return 2" `matches` "public function build($a, $b) {\n    return 2;\n}",
    "@@build a b := return 2" `matches` "public static function build($a, $b) {\n    return 2;\n}",
    "incr a := return a + 1" `matches` "public function incr($a) {\n    return $a + 1;\n}",
    "foo := a.foo()" `matches` "public function foo() {\n    $a->foo();\n}",
    "foo a b := a + 1 + b + 2" `matches` "public function foo($a, $b) {\n    $a + 1 + $b + 2;\n}",
    "foo a b := (a + 1) + (b - 2)" `matches` "public function foo($a, $b) {\n    ($a + 1) + ($b - 2);\n}",

    -- parens tests
    "(a + b)" `matches` "($a + $b);",
    "((a + b))" `matches` "(($a + $b));",
    "(((a + b)))" `matches` "((($a + $b)));",
    "(((foo())))" `matches` "(((foo())));",
    "(((a.foo())))" `matches` "((($a->foo())));",
    "((a + 1) * (b - 4))" `matches` "(($a + 1) * ($b - 4));",

    -- braces tests
    "fib x := {\nif x < 2 then {\nreturn x\n} else {\nreturn fib(x - 1) + fib(x - 2)\n}\n}" `matches` "public function fib($x) {\n    if ($x < 2) {\n        return $x;\n    } else {\n        return fib($x - 1) + fib($x - 2);\n    }\n}",
    "fib x := {\na + b\n b + c\n }\n \n foo a b := a + b" `matches` "public function fib($x) {\n    $a + $b;\n    $b + $c;\n}\npublic function foo($a, $b) {\n    $a + $b;\n}",
    "fib x := {\na + b\n b + c\n }\n \n foo a b := { a + b }" `matches` "public function fib($x) {\n    $a + $b;\n    $b + $c;\n}\npublic function foo($a, $b) {\n    $a + $b;\n}",

    -- hash tests
    "argv[1]" `matches` "$argv[1];",
    "argv[1][2]" `matches` "$argv[1][2];",
    "fib(argv[1])" `matches` "fib($argv[1]);",
    "var_dump(fib(argv[1]))" `matches` "var_dump(fib($argv[1]));",

    -- if statement
    "if a = 1 then {\n b = 2\n c = 3\n }" `matches`"if ($a = 1) {\n    $b = 2;\n    $c = 3;\n}",
    "if a != 'foo' then return 2 else return 3" `matches` "if ($a != \"foo\") {\n    return 2;\n} else {\n    return 3;\n}",

    -- while statement
    "while foo == 1 {\nfoo = 2\n}" `matches`"while ($foo == 1) {\n    $foo = 2;\n}",
    "while foo == 1 {\nfoo = 2\nbar = 3\n}" `matches`"while ($foo == 1) {\n    $foo = 2;\n    $bar = 3;\n}",

    -- function calls
    "Blocklist.foo()" `matches` "Blocklist::foo();",
    "a.foo()" `matches` "$a->foo();",
    "@a.foo()" `matches` "$this->a->foo();",
    "@foo()" `matches` "$this->foo();",
    "@@foo()" `matches` "static::foo();",

    "Blocklist.foo(1, 2)" `matches` "Blocklist::foo(1, 2);",
    "a.foo(1, 2)" `matches` "$a->foo(1, 2);",
    "@a.foo(1, 2)" `matches` "$this->a->foo(1, 2);",
    "@foo(1, 2)" `matches` "$this->foo(1, 2);",
    "@@foo(1, 2)" `matches` "static::foo(1, 2);",

    "Blocklist.foo(b)" `matches` "Blocklist::foo($b);",
    "a.foo(b)" `matches` "$a->foo($b);",
    "@a.foo(b)" `matches` "$this->a->foo($b);",
    "@foo(b)" `matches` "$this->foo($b);",
    "@@foo(b)" `matches` "static::foo($b);",

    "Blocklist.foo(b.bar())" `matches` "Blocklist::foo($b->bar());",
    "a.foo(b.bar())" `matches` "$a->foo($b->bar());",
    "@a.foo(@bar())" `matches` "$this->a->foo($this->bar());",
    "@foo(@@bar())" `matches` "$this->foo(static::bar());",
    "@@foo(@b.bar())" `matches` "static::foo($this->b->bar());",

    -- negate
    "!foo" `matches` "!$foo;",

    -- class definition
    "class Blocklist {\n @foo := p(\"hi!\")\n }" `matches` "class Blocklist {\n    public function foo() {\n        var_dump(\"hi!\");\n    }\n}",

    -- object creation
    "class Blocklist {\n@foo := p(\"hi!\")\n }\n b = new Blocklist()\n b.foo()" `matches` "class Blocklist {\n    public function foo() {\n        var_dump(\"hi!\");\n    }\n}\n$b = new Blocklist();\n$b->foo();",

    -- feature flag
    "~foo.bar" `matches` "Feature::isEnabled('foo.bar');",

    -- null, true, false
    "a = true" `matches` "$a = true;",
    "b = false" `matches` "$b = false;",
    "c = null" `matches` "$c = null;",
    "_SAMPLE_RATE = 0.001" `matches` "private const SAMPLE_RATE = 0.001;",
    "MYCONST = 'foo'" `matches` "public const MYCONST = \"foo\";",
    "a = {\n foo: 1,\n bar: 2,\n cat: 'hello',\n }" `matches`"$a = {\n    foo => 1,\n    bar => 2,\n    cat => \"hello\"\n}"
    -- "arr.any(\\x -> x + 1)" `matches`"$result = false;\nforeach ($arr as $x) {\nif(x + 1) {\n$result = true;\nbreak;\n}"
    -- "arr.any(&even)" `matches`"$result = false;\nforeach ($arr as $i) {\nif(even($i)) {\n$result = true;\nbreak;\n}",
    -- "arr.any(&@even)" `matches`"$result = false;\nforeach ($arr as $i) {\nif($i->even()) {\n$result = true;\nbreak;\n}",
    -- "@adit.map(\\x -> x + 1)" `matches` "$result = [];\nforeach ($this->adit as $x) {\n$result []= x + 1;\n}"
    -- "fib x := return x if x < 2" `matches` "function fib($x) {\nif ($x < 2) {\nreturn $x;\n}"
    -- "@@foo a b := @@bar(b)" `matches` "static function foo($a, $b) {\n\treturn static::bar($b);\n}",
    -- "# hi" `matches` "// hi",
    -- "~loc.flag" `matches` "Feature::isEnabled('loc.flag')",
    -- matches [r|
    -- foo :: []?, EP_Locale? -> String
    -- foo a b := @@bar(a, b)
  -- |] [r|
    -- /**
    --  * @param array|null $a
    --  * @param EP_Locale|null $b
    --  * @return string
    --  */
    --  function foo(?array $a = null, ?EP_Locale $b = null) {
    --     return static::bar($a, $b);
    --  }
    -- |],
    -- "@loc.getLanguage + @loc.getRegion" `matches` "$loc->getLanguage() . $loc->getRegion()",
    -- "a += 1" `matches` "$a = $a + 1",
    -- "p 'hello' if hash ? str" `matches` [r|
    --   if (isset($hash['str'])) {
    --     var_dump 'hello';
    --     }|],
    -- "hash > str" `matches` "$hash[$str]",
    -- "hash > str > str2 > str3" `matches` "$hash[$str][$str2][$str3]",
    -- "arr.map(\b acc -> b+1)" `matches` [r|
    --   $acc = [];
    --   foreach ($arr as $b) {
    --     $acc []= $b+1;
    --   }
    -- |],
    -- "arr.filter \b acc -> b > :is_stem" `matches` [r|
    --   $acc = [];
    --   foreach ($arr as $b) {
    --     if ($b['is_stem']) {
    --       $acc []= $b;
    --     }
    --   }
    -- |],
    -- [r|arr.each \b ->
    --   b[word] <$> \ids ->
    --       ids.each \id ->
    --         hits[id] ||= 0
    --         hits[id] += 1

    -- |] `matches` [r|
    --   foreach ($b as $arr) {
    --     if (isset($b[$word])) {
    --       $ids = $b[$word];
    --       foreach ($ids as $id) {
    --         $hits[$id] = $hits[$id] ?? 0;
    --         $hits[$id] = $hits[$id] + 1;
    --       }
    --     }
    --   }
    -- |],
    -- "arr.any \b acc -> b > :is_stem" `matches` [r|
    --   $acc = false;
    --   foreach ($arr as $b) {
    --     if ($b['is_stem']) {
    --       $acc = true;
    --       break;
    --     }
    --   }
    -- |],
    -- "arr.all \b acc -> b > :is_stem" `matches` [r|
    --   $acc = true;
    --   foreach ($arr as $b) {
    --     if (!$b['is_stem']) {
    --       $acc = false;
    --       break;
    --     }
    --   }
    -- |],
    -- "return @@memcache.allCacheKeys(@@buildKey(blocklists, loc || default_loc()))" `matches` [r|
    --   return static::memcache()->allCacheKeys(static::buildKey($blocklists, $loc ?? default_loc()));
    -- |],
    -- "return @@memcache.allCacheKeys $ @@buildKey(blocklists, loc || default_loc())" `matches` [r|
    --   return static::memcache()->allCacheKeys(static::buildKey($blocklists, $loc ?? default_loc()));
    -- |],
    -- "b ||= []" `matches` "$b = $b ?? []",
    -- "Blocklist lists" `matches` "new Blocklist($lists)",
    -- "foo.uniq" `matches` "array_unique($foo)",
    -- "array_merge $ a b c" `matches` "array_merge($a, $b, $c)"
  ]

-- assignmentTests = map makeToPhpTest $ [
--     -- equals
--     (Assignment (SimpleVar "foo") Equals (SaltyNumber "1"), "$foo = 1"),
--     (Assignment (InstanceVar "foo") Equals (SaltyNumber "1"), "$this->foo = 1"),
--     (Assignment (ClassVar "foo") Equals (SaltyNumber "1"), "self::foo = 1"),

--     -- plusequals
--     (Assignment (SimpleVar "foo") PlusEquals (SaltyNumber "1"), "$foo = $foo + 1"),
--     (Assignment (InstanceVar "foo") PlusEquals (SaltyNumber "1"), "$this->foo = $this->foo + 1"),
--     (Assignment (ClassVar "foo") PlusEquals (SaltyNumber "1"), "self::foo = self::foo + 1"),

--     -- minusequals
--     (Assignment (SimpleVar "foo") MinusEquals (SaltyNumber "1"), "$foo = $foo - 1"),
--     (Assignment (InstanceVar "foo") MinusEquals (SaltyNumber "1"), "$this->foo = $this->foo - 1"),
--     (Assignment (ClassVar "foo") MinusEquals (SaltyNumber "1"), "self::foo = self::foo - 1"),

--     -- orequals
--     (Assignment (SimpleVar "foo") OrEquals (SaltyNumber "1"), "$foo = $foo ?? 1"),
--     (Assignment (InstanceVar "foo") OrEquals (SaltyNumber "1"), "$this->foo = $this->foo ?? 1"),
--     (Assignment (ClassVar "foo") OrEquals (SaltyNumber "1"), "self::foo = self::foo ?? 1")
--   ]

-- functionTests = map makeToPhpTest $ [
--     -- different types of functions (instance vs class)
--     (Function (SimpleVar "foo") [Argument (Just "array") "arg1" Nothing] (OneLine $ SaltyNumber "1"), "function foo(array $arg1) {\n1;\n}"),
--     (Function (InstanceVar "foo") [Argument (Just "array") "arg1" Nothing] (OneLine $ SaltyNumber "1"), "function foo(array $arg1) {\n1;\n}"),
--     (Function (ClassVar "foo") [Argument (Just "array") "arg1" Nothing] (OneLine $ SaltyNumber "1"), "static function foo(array $arg1) {\n1;\n}"),

--     -- different numbers of args
--     (Function (InstanceVar "foo") [Argument (Just "array") "arg1" Nothing, Argument (Just "string") "arg2" Nothing] (OneLine $ SaltyNumber "1"), "function foo(array $arg1, string $arg2) {\n1;\n}"),
--     (Function (InstanceVar "foo") [Argument (Just "array") "arg1" Nothing, Argument (Just "string") "arg2" Nothing, Argument (Just "EP_Locale") "loc" Nothing] (OneLine $ SaltyNumber "1"), "function foo(array $arg1, string $arg2, EP_Locale $loc) {\n1;\n}"),

--     -- default args
--     (Function (InstanceVar "foo") [Argument (Just "array") "arg1" Nothing, Argument (Just "string") "arg2" $ Just "null"] (OneLine $ SaltyNumber "1"), "function foo(array $arg1, ?string $arg2 = null) {\n1;\n}"),

--     -- different types of function bodies
--     (Function (SimpleVar "foo") [Argument (Just "array") "arg1" Nothing] (OneLine $ Assignment (SimpleVar "foo") Equals (SaltyNumber "1")), "function foo(array $arg1) {\n$foo = 1;\n}"),
--     (Function (SimpleVar "foo") [Argument (Just "array") "arg1" Nothing] (Block [Assignment (SimpleVar "foo") Equals (SaltyNumber "1"), Assignment (SimpleVar "foo") PlusEquals (SaltyNumber "1")]), "function foo(array $arg1) {\n$foo = 1;\n$foo = $foo + 1;\n}"),
--     (Function (SimpleVar "foo") [Argument (Just "array") "arg1" Nothing] (AmpersandFunction (SimpleVar "foo")), "ampersand function body not allowed as method body SimpleVar \"foo\""),
--     -- (Function (SimpleVar "foo") [Argument (Just "array") "arg1" Nothing] (LambdaFunction ["a", "b"] $ AmpersandFunction (SimpleVar "foo")), "lambda function body not allowed as method body SimpleVar \"foo\""),

--     (SaltyNumber "123", "123"),
--     (SaltyNumber "123.0", "123.0"),
--     (SaltyString "hello there", "hello there"),
--     -- function calls
--     -- type of function (instance/class)
--     (FunctionCall Nothing (SimpleVar "foo") [], "foo()"),
--     (FunctionCall Nothing (InstanceVar "foo") [], "$this->foo()"),
--     (FunctionCall Nothing (ClassVar "foo") [], "static::foo()"),

--     -- calls on an object, function is simplevar
--     (FunctionCall (Just $ SimpleVar "obj") (SimpleVar "foo") [], "$obj->foo()"),
--     (FunctionCall (Just $ InstanceVar "obj") (SimpleVar "foo") [], "$this->obj->foo()"),
--     (FunctionCall (Just $ ClassVar "obj") (SimpleVar "foo") [], "static::obj->foo()"),

--     -- calls on an object, function is instancevar. No difference from simplevar.
--     (FunctionCall (Just $ SimpleVar "obj") (InstanceVar "foo") [], "$obj->foo()"),
--     (FunctionCall (Just $ InstanceVar "obj") (InstanceVar "foo") [], "$this->obj->foo()"),
--     (FunctionCall (Just $ ClassVar "obj") (InstanceVar "foo") [], "static::obj->foo()"),

--     -- calls on an object, function is classvar. No difference from simplevar.
--     (FunctionCall (Just $ SimpleVar "obj") (ClassVar "foo") [], "$obj->foo()"),
--     (FunctionCall (Just $ InstanceVar "obj") (ClassVar "foo") [], "$this->obj->foo()"),
--     (FunctionCall (Just $ ClassVar "obj") (ClassVar "foo") [], "static::obj->foo()"),

--     -- pass args to function calls (single arg)
--     (FunctionCall Nothing (SimpleVar "foo") ["1"], "foo(1)"),
--     (FunctionCall Nothing (InstanceVar "foo") ["1"], "$this->foo(1)"),
--     (FunctionCall Nothing (ClassVar "foo") ["1"], "static::foo(1)"),

--     -- pass args to function calls (multiple args)
--     (FunctionCall Nothing (SimpleVar "foo") ["1", "2"], "foo(1, 2)"),
--     (FunctionCall Nothing (InstanceVar "foo") ["1", "2"], "$this->foo(1, 2)"),
--     (FunctionCall Nothing (ClassVar "foo") ["1", "2"], "static::foo(1, 2)"),

--     -- higher order calls
--     (HigherOrderFunctionCall (SimpleVar "foo") Each (AmpersandFunction (SimpleVar "funcName")), "foreach ($foo as $i) {\nfuncName($i);\n}"),
--     (HigherOrderFunctionCall (SimpleVar "foo") Each (AmpersandFunction (InstanceVar "funcName")), "foreach ($foo as $i) {\n$i->funcName();\n}"),
--     (HigherOrderFunctionCall (SimpleVar "foo") Each (LambdaFunction ["a"] (SaltyNumber "1")), "foreach ($foo as $a) {\n1;\n}"),
--     (HigherOrderFunctionCall (SimpleVar "foo") Each (LambdaFunction ["a"] (Assignment (SimpleVar "a") Equals (SaltyNumber "1"))), "foreach ($foo as $a) {\n$a = 1;\n}"),

--     (HigherOrderFunctionCall (SimpleVar "foo") Map (AmpersandFunction (SimpleVar "funcName")), "$acc = [];\nforeach ($foo as $i) {\n$acc []= funcName($i);\n}"),
--     (HigherOrderFunctionCall (SimpleVar "foo") Map (AmpersandFunction (InstanceVar "funcName")), "$acc = [];\nforeach ($foo as $i) {\n$acc []= $i->funcName();\n}"),
--     (HigherOrderFunctionCall (SimpleVar "foo") Map (LambdaFunction ["a", "newList"] (SaltyNumber "1")), "$newList = [];\nforeach ($foo as $a) {\n$newList []= 1;\n}"),
--     (HigherOrderFunctionCall (SimpleVar "foo") Map (LambdaFunction ["a", "newList"] (Assignment (SimpleVar "a") Equals (SaltyNumber "1"))), "$newList = [];\nforeach ($foo as $a) {\n$newList []= ($a = 1);\n}"),

--     -- hash lookup
--     (HashLookup (Left (SimpleVar "hash")) (SimpleVar "key"), "$hash[$key]"),
--     (HashLookup (Left (InstanceVar "hash")) (InstanceVar "key"), "$this->hash[$this->key]"),
--     (HashLookup (Left (ClassVar "hash")) (ClassVar "key"), "static::$hash[static::$key]"),
--     (HashLookup (Right (HashLookup (Left (InstanceVar "hash")) (InstanceVar "key"))) (ClassVar "key2"), "$this->hash[$this->key][static::$key2]")
--   ]

allTests = TestList $
              transpileTests
            -- assignmentTests
            -- ++ functionTests


main :: IO ()
main = runTestTT allTests >> return ()
