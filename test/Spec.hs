{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ

import Test.HUnit
import Lib
import Types

matches str1 str2 = TestCase $ assertEqual "" str2 (build str1)

makeToPhpTest :: (Salty, String) -> Test
makeToPhpTest (salty,expectedStr) = (toPhp salty) `matches` expectedStr

tests = TestList [
    -- "@foo = 1" `matches` "$this->foo = 1",
    -- "@@foo = 1" `matches` "self::$foo = 1",
    -- "build a b := 2" `matches` "function build($a, $b) {\nreturn 2;\n}",
    -- "build a b := return 2" `matches` "function build($a, $b) {\nreturn 2;\n}",
    -- "fib x := return x if x < 2" `matches` "function fib($x) {\nif ($x < 2) {\nreturn $x;\n}"
    -- "@@build a b := 2" `matches` "static function build($a, $b) {\n\treturn 2;\n}",
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

assignmentTests = map makeToPhpTest $ [
    -- equals
    (Assignment (SimpleVar "foo") Equals (SaltyNumber "1"), "$foo = 1"),
    (Assignment (InstanceVar "foo") Equals (SaltyNumber "1"), "$this->foo = 1"),
    (Assignment (ClassVar "foo") Equals (SaltyNumber "1"), "self::foo = 1"),

    -- plusequals
    (Assignment (SimpleVar "foo") PlusEquals (SaltyNumber "1"), "$foo = $foo + 1"),
    (Assignment (InstanceVar "foo") PlusEquals (SaltyNumber "1"), "$this->foo = $this->foo + 1"),
    (Assignment (ClassVar "foo") PlusEquals (SaltyNumber "1"), "self::foo = self::foo + 1"),

    -- minusequals
    (Assignment (SimpleVar "foo") MinusEquals (SaltyNumber "1"), "$foo = $foo - 1"),
    (Assignment (InstanceVar "foo") MinusEquals (SaltyNumber "1"), "$this->foo = $this->foo - 1"),
    (Assignment (ClassVar "foo") MinusEquals (SaltyNumber "1"), "self::foo = self::foo - 1"),

    -- orequals
    (Assignment (SimpleVar "foo") OrEquals (SaltyNumber "1"), "$foo = $foo ?? 1"),
    (Assignment (InstanceVar "foo") OrEquals (SaltyNumber "1"), "$this->foo = $this->foo ?? 1"),
    (Assignment (ClassVar "foo") OrEquals (SaltyNumber "1"), "self::foo = self::foo ?? 1")
  ]

functionTests = map makeToPhpTest $ [
    -- different types of functions
    (Function (SimpleVar "foo") [Argument "array" "arg1" Nothing] (OneLine $ SaltyNumber "1"), "function foo(array $arg1) {\n1\n}")
  ]

allTests = TestList $
            assignmentTests
            ++ functionTests


main :: IO ()
main = runTestTT allTests >> return ()
