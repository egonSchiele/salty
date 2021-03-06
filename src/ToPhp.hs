module ToPhp where
import Types
import Print
import Data.List (intercalate)
import Utils (isConstant, join, stripNewline)

initToPhp body = join "\n" $ map toPhp (init body)
stripNewlineToPhp (WithNewLine salty) = toPhp salty
stripNewlineToPhp salty = toPhp salty

simpleVarName :: VariableName -> String
simpleVarName x = case x of
    InstanceVar s -> s
    StaticVar s -> s
    ClassVar s -> s
    SimpleVar s -> s

formatLoopVars [] = "$x"
formatLoopVars (x:[]) = "$" ++ x
formatLoopVars (x:y:[]) = print2 "$% => $%" x y

addReturnToArray :: [Salty] -> String
addReturnToArray [] = ""
addReturnToArray (x:[]) = addReturn x
addReturnToArray x = ((join "\n") . map toPhp . init $ x) ++ "\n" ++ (addReturn . last $ x)

varName :: Salty -> String
varName (Variable name scope) = toPhp name
varName x = "this shouldnt be in varName: " ++ (show x)

varNameToFunc (InstanceVar s) = "$this->" ++ s
varNameToFunc (StaticVar s) = "static::" ++ s
varNameToFunc (ClassVar s) = "error classvar as funcname: " ++ s
varNameToFunc (SimpleVar s) = s

class ConvertToPhp a where
    toPhp :: a -> String

instance ConvertToPhp VariableName where
  toPhp (InstanceVar s) = "$this->" ++ s
  toPhp (StaticVar s) = "static::$" ++ s
  toPhp (ClassVar s) = s
  toPhp (SimpleVar s) = '$':s

instance ConvertToPhp ArgumentType where
  toPhp (ArgumentType True typ False) = "@param " ++ typ ++ "|null"
  toPhp (ArgumentType False typ False) = "@param " ++ typ
  toPhp (ArgumentType True typ True) = "@return " ++ typ ++ "|null"
  toPhp (ArgumentType False typ True) = "@return " ++ typ

instance ConvertToPhp Visibility where
  toPhp Public = "public"
  toPhp Private = "private"

instance ConvertToPhp MagicConstant where
  toPhp MCLINE = "__LINE__"
  toPhp MCFILE = "__FILE__"
  toPhp MCDIR = "__DIR__"
  toPhp MCFUNCTION = "__FUNCTION__"
  toPhp MCCLASS = "__CLASS__"
  toPhp MCTRAIT = "__TRAIT__"
  toPhp MCMETHOD = "__METHOD__"
  toPhp MCNAMESPACE = "__NAMESPACE__"

instance ConvertToPhp ArgumentName where
  toPhp (ArgumentName name False)
    | (take 3 name == "...") = "...$" ++ (drop 3 name)
    | otherwise = "$" ++ name
  toPhp (ArgumentName name True) = "&$" ++ name
instance ConvertToPhp Argument where
  toPhp (Argument (Just (ArgumentType False typ _)) name (Just default_)) = print3 "?% % = %" typ (toPhp name) default_
  toPhp (Argument (Just (ArgumentType True typ _)) name (Just default_)) = print3 "?% % = %" typ (toPhp name) default_
  toPhp (Argument (Just (ArgumentType False typ _)) name Nothing) = typ ++ " " ++ (toPhp name)
  toPhp (Argument (Just (ArgumentType True typ _)) name Nothing) = print2 "?% % = null" typ (toPhp name)
  toPhp (Argument Nothing name (Just default_)) = print2 "% = %" (toPhp name) default_
  toPhp (Argument Nothing name Nothing) = toPhp name

instance (ConvertToPhp a1, ConvertToPhp a2) => ConvertToPhp (Either a1 a2) where
  toPhp (Left a) = toPhp a
  toPhp (Right a) = toPhp a


instance ConvertToPhp Salty where
  toPhp (Operation x op (WithNewLine y)) = (toPhp $ Operation x op y) ++ "\n"
  toPhp (Operation x@(Variable _ _) Equals (HigherOrderFunctionCall obj callName func accVar)) = toPhp $ HigherOrderFunctionCall obj callName func (varName x)
  toPhp (Operation x@(Variable _ _) Equals (FunctionCall (Just (HigherOrderFunctionCall obj callName func accVar)) fName args block)) = toPhp $ FunctionCall (Just $ HigherOrderFunctionCall obj callName func (varName x)) fName args block

  -- this is a hack -- it's the same as the statement above just w the WithNewLine added.
  -- toPhp (Operation x@(Variable _ _) Equals (WithNewLine (HigherOrderFunctionCall obj callName func accVar))) = toPhp $ HigherOrderFunctionCall obj callName func (varName x)
  toPhp (Operation left Equals (If cond thenPath_ (Just elsePath_))) = print4 "% = % ? % : %" (toPhp left) (concat . map toPhp $ cond) (toPhp thenPath_) (toPhp elsePath_)
  toPhp (Operation left Equals (If cond thenPath_ Nothing)) = print3 "% = null;\nif (%) {\n%\n}" (toPhp left) (concat . map toPhp $ cond) (toPhp (Operation left Equals thenPath_))
  toPhp (Operation (ArraySlice obj start Nothing) Equals arr) = print3 "array_splice(%, %, null, %)" (toPhp obj) (toPhp start) (toPhp arr)
  toPhp (Operation (ArraySlice obj (SaltyNumber start) (Just (SaltyNumber end))) Equals arr) = print4 "array_splice(%, %, %, %)" (toPhp obj) start newEnd (toPhp arr)
    where newEnd = show $ (read end :: Integer) - (read start :: Integer)
  toPhp (Operation (ArraySlice obj start (Just end)) Equals arr) = print5 "array_splice(%, %, % - %, %)" (toPhp obj) (toPhp start) (toPhp end) (toPhp start) (toPhp arr)
  toPhp (Operation (DestructuredHash vars) Equals right) = join "\n" $ map assignVars vars
    where assignVars v = print3 "$% = %['%']" v (toPhp right) v
  toPhp (Operation left Equals right) = (toPhp left) ++ " = " ++ (toPhp right)
  toPhp (Operation left NotEquals right) = (toPhp left) ++ " != " ++ (toPhp right)
  toPhp (Operation left PlusEquals right) = print3 "% = % + %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left MinusEquals right) = print3 "% = % - %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left MultiplyEquals right) = print3 "% = % * %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left DivideEquals right) = print3 "% = % / %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left OrEquals right) = print3 "% = % ?? %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left ArrayPush right) = print2 "% []= %" (toPhp left) (toPhp right)
  toPhp (Operation left Add right) = print2 "% + %" (toPhp left) (toPhp right)
  toPhp (Operation left Subtract right) = print2 "% - %" (toPhp left) (toPhp right)
  toPhp (Operation left Divide right) = print2 "% / %" (toPhp left) (toPhp right)
  toPhp (Operation left Modulo right) = (toPhp left) ++ " % " ++ (toPhp right)
  toPhp (Operation left Multiply right) = print2 "% * %" (toPhp left) (toPhp right)
  toPhp (Operation left OrOr right) = print2 "% || %" (toPhp left) (toPhp right)
  toPhp (Operation left AndAnd right) = print2 "% && %" (toPhp left) (toPhp right)
  toPhp (Operation left NullCoalesce right) = print2 "% ?? %" (toPhp left) (toPhp right)
  toPhp (Operation left PlusPlus right) = print2 "% . %" (toPhp left) (toPhp right)
  toPhp (Operation left ArrayMerge right) = print2 "array_merge(%, %)" (toPhp left) (toPhp right)
  toPhp (Operation left ArrayDiff right) = print2 "array_diff(%, %)" (toPhp left) (toPhp right)
  toPhp (Operation left In (Range start end)) = print4 "% >= % && % <= %" (toPhp left) (toPhp start) (toPhp left) (toPhp end)
  toPhp (Operation left In (Parens [Range start end])) = print4 "% >= % && % <= %" (toPhp left) (toPhp start) (toPhp left) (toPhp end)
  toPhp (Operation left In right) = print2 "in_array(%, %)" (toPhp left) (toPhp right)
  toPhp (Operation left KeyIn right) = print2 "array_key_exists(%, %)" (toPhp left) (toPhp right)
  toPhp (Operation left InstanceOf right) = print2 "% instanceof %" (toPhp left) (toPhp right)
  toPhp (Operation left EqualsEquals right) = print2 "% == %" (toPhp left) (toPhp right)
  toPhp (Operation left LessThan right) = print2 "% < %" (toPhp left) (toPhp right)
  toPhp (Operation left LessThanOrEqualTo right) = print2 "% <= %" (toPhp left) (toPhp right)
  toPhp (Operation left GreaterThan right) = print2 "% > %" (toPhp left) (toPhp right)
  toPhp (Operation left GreaterThanOrEqualTo right) = print2 "% >= %" (toPhp left) (toPhp right)
  toPhp (Operation left Spaceship right) = print2 "% <=> %" (toPhp left) (toPhp right)

  toPhp (Function name args body visibility scope) = print4 "%%(%) {\n%\n}\n" visibilityToDisplay funcName funcArgs funcBody
    where funcName = case name of
            InstanceVar str -> "function " ++ str
            StaticVar str -> "static function " ++ str
            SimpleVar str -> "function " ++ str
          visibilityToDisplay = case scope of
                                     ClassScope -> (toPhp visibility) ++ " "
                                     _ -> ""
          funcArgs = intercalate ", " $ map toPhp args
          funcBody = case body of
                          [] -> ""
                          [Braces []] -> ""
                          [Braces salties] -> print2 "%\n%" ((join "\n") . map toPhp . init $ salties) (addReturn . last $ salties)
                          x -> print2 "%\n%" ((join "\n") . map toPhp . init $ x) (addReturn . last $ x)

  toPhp (SaltyNumber s) = s
  toPhp (SaltyString s) = "\"" ++ s ++ "\""

  -- functions called without an object (bare)
  toPhp (FunctionCall Nothing (Right var) args Nothing) = print2 "%(%)" (varNameToFunc var) (intercalate ", " . map toPhp $ args)

  toPhp (FunctionCall Nothing (Right var) args (Just block@(Braces arr))) = print2 "%(%)" (varNameToFunc var) args_
    where args_ = join ", " $ (map toPhp (args ++ (map stripNewline arr)))

  toPhp (FunctionCall Nothing (Right var) args (Just block@(LambdaFunction _ _))) = print2 "%(%)" (varNameToFunc var) args_
    where args_ = join ", " $ (map toPhp args) ++ [(addReturn block)]

  -- builtin bare functions
  toPhp (FunctionCall Nothing (Left VarDumpShort) args _) = "var_dump(" ++ (intercalate ", " . map toPhp $ args) ++ ")"

  -- functions called on higher order functions
  toPhp (FunctionCall (Just hof@(HigherOrderFunctionCall _ _ _ accVar)) (Right funcName) args block) = print3 "%\n% = %" (toPhp hof) accVar (toPhp (FunctionCall (Just (PurePhp accVar)) (Right funcName) args block))
  -- builtin functions on an obj
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "split")) [] _) = print2 "explode('%', %)" " " (toPhp obj)
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "join")) [] _) = print2 "implode('%', %)" " " (toPhp obj)
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "split")) [SaltyString separator] _) = print2 "explode('%', %)" separator (toPhp obj)
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "join")) [SaltyString separator] _) = print2 "implode('%', %)" separator (toPhp obj)
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "uniq")) [] _) = "array_unique(" ++ (toPhp obj) ++ ")"
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "pop")) [] _) = "array_pop(" ++ (toPhp obj) ++ ")"
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "keys")) [] _) = "array_keys(" ++ (toPhp obj) ++ ")"
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "values")) [] _) = "array_values(" ++ (toPhp obj) ++ ")"
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "reverse")) [] _) = "array_reverse(" ++ (toPhp obj) ++ ")"
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "count")) [] _) = "count(" ++ (toPhp obj) ++ ")"
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "size")) [] _) = "count(" ++ (toPhp obj) ++ ")"
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "shuffle")) [] _) = "shuffle(" ++ (toPhp obj) ++ ")"
  toPhp (FunctionCall (Just obj) (Right (SimpleVar "sub")) [search, replace] _) = print3 "str_replace(%, %, %)" (toPhp search) (toPhp replace) (toPhp obj)
  toPhp (FunctionCall (Just (Variable vName _)) (Right (SimpleVar "new")) args (Just (Braces block))) = toPhp $ New vName (args ++ (map stripNewline block))
  toPhp (FunctionCall (Just (Variable vName _)) (Right (SimpleVar "new")) args _) = toPhp $ New vName args

  -- functions called on an obj
  toPhp (FunctionCall (Just (Variable (ClassVar obj) _)) (Right funcName) args Nothing) = print3 "%::%(%)" obj (simpleVarName funcName) (intercalate ", " . map toPhp $ args)
  toPhp (FunctionCall (Just var@(Variable _ _)) (Right funcName) args Nothing) = print3 "%->%(%)" (toPhp var) (simpleVarName funcName) (intercalate ", " . map toPhp $ args)


  toPhp (FunctionCall (Just obj) (Right funcName) args Nothing) = print3 "%->%(%)" (toPhp obj) (simpleVarName funcName) (intercalate ", " . map toPhp $ args)

  toPhp (FunctionCall (Just var@(Variable _ _)) (Right funcName) args (Just block@(Braces arr))) = print3 "%->%(%)" (toPhp var) (simpleVarName funcName) args_
    where args_ = join ", " $ (map toPhp $ args ++ (map stripNewline arr))

  toPhp (FunctionCall (Just var@(Variable _ _)) (Right funcName) args (Just block@(LambdaFunction _ _))) = print3 "%->%(%)" (toPhp var) (simpleVarName funcName) args_
    where args_ = join ", " $ (map toPhp args) ++ [(addReturn block)]

  -- same as above but with parens
  toPhp (FunctionCall (Just (Parens [obj])) (Right funcName) args _) = print3 "(%)->%(%)" (toPhp obj) (simpleVarName funcName) (intercalate ", " . map toPhp $ args)

  toPhp (LambdaFunction args body) = print2 "function(%) {\n%\n}" (join ", " (map (\a -> '$':a) args)) (addReturn body)

  -- special case, each with a range
  toPhp (HigherOrderFunctionCall (Range left right) Each (LambdaFunction loopVar body) _)  =
                print6 "for (% = %; % <= %; %++) {\n%\n}" (formatLoopVars loopVar) (toPhp left) (formatLoopVars loopVar) (toPhp right) (formatLoopVars loopVar) (toPhp body)
  toPhp (HigherOrderFunctionCall (Parens [Range left right]) Each (LambdaFunction loopVar body) _)  =
                print6 "for (% = %; % <= %; %++) {\n%\n}" (formatLoopVars loopVar) (toPhp left) (formatLoopVars loopVar) (toPhp right) (formatLoopVars loopVar) (toPhp body)

  -- each
  toPhp (HigherOrderFunctionCall obj Each (LambdaFunction loopVar body) _)  =
                print3 "foreach (% as %) {\n%;\n}\n" (toPhp obj) (formatLoopVars loopVar) (toPhp body)

  -- map
  toPhp (HigherOrderFunctionCall obj Map (LambdaFunction loopVar (Braces body)) accVar) =
                print6 "% = [];\nforeach (% as %) {\n%\n% []= %;\n}\n" accVar (toPhp obj) (formatLoopVars loopVar) (initToPhp body) accVar (stripNewlineToPhp (last body))

  toPhp (HigherOrderFunctionCall obj Map (LambdaFunction loopVar body) accVar) =
                print5 "% = [];\nforeach (% as %) {\n% []= %;\n}\n" accVar (toPhp obj) (formatLoopVars loopVar) accVar (toPhp body)

  -- select
  toPhp (HigherOrderFunctionCall obj Select (LambdaFunction loopVar (Braces body)) accVar) =
                accVar ++ " = [];\n" ++ (print6 "foreach (% as %) {\n%\nif(%) {\n% []= %;\n}\n}\n" (toPhp obj) (formatLoopVars loopVar) (initToPhp body) (stripNewlineToPhp . last $ body) accVar (formatLoopVars loopVar))

  toPhp (HigherOrderFunctionCall obj Select (LambdaFunction loopVar body) accVar) =
                print6 "% = [];\nforeach (% as %) {\nif(%) {\n% []= %;\n}\n}\n" accVar (toPhp obj) (formatLoopVars loopVar) (toPhp body) accVar (formatLoopVars loopVar)

  -- any
  toPhp (HigherOrderFunctionCall obj Any (LambdaFunction loopVar (Braces body)) accVar) =
                print6 "% = false;\nforeach (% as %) {\n%\nif(%) {\n% = true;\nbreak;\n}\n}\n" accVar (toPhp obj) (formatLoopVars loopVar) (initToPhp body) (stripNewlineToPhp . last $ body) accVar

  toPhp (HigherOrderFunctionCall obj Any (LambdaFunction loopVar body) accVar) =
                print5 "% = false;\nforeach (% as %) {\nif(%) {\n% = true;\nbreak;\n}\n}\n" accVar (toPhp obj) (formatLoopVars loopVar) (toPhp body) accVar

  -- all
  toPhp (HigherOrderFunctionCall obj All (LambdaFunction loopVar (Braces body)) accVar) =
                print6 "% = true;\nforeach (% as %) {\n%\nif(!%) {\n% = false;\nbreak;\n}\n}\n" accVar (toPhp obj) (formatLoopVars loopVar) (initToPhp body) (stripNewlineToPhp . last $ body) accVar

  toPhp (HigherOrderFunctionCall obj All (LambdaFunction loopVar body) accVar) =
                print5 "% = true;\nforeach (% as %) {\nif(!%) {\n% = false;\nbreak;\n}\n}\n" accVar (toPhp obj) (formatLoopVars loopVar) (toPhp body) accVar

  toPhp Salt = "I'm salty"
  toPhp (ReturnStatement s) = "return " ++ (toPhp s)
  toPhp (ReturnStatementForAddReturn s) = addReturn s
  toPhp (Parens s) = "(" ++ (concat $ map toPhp s) ++ ")"
  toPhp (Braces s) = join "\n" $ map toPhp s
  toPhp (PurePhp line) = line
  toPhp (PhpComment str) = "// " ++ str ++ "\n"
  toPhp (SaltyComment str) = ""
  toPhp (Negate s) = "!" ++ (toPhp s)
  toPhp EmptyLine = ""
  toPhp (BackTrack s) = toPhp s

  toPhp (If cond thenFork (Just elseFork)) = print3 "if (%) {\n%\n} else {\n%\n}" (concat . map toPhp $ cond) (toPhp thenFork) (toPhp elseFork)
  toPhp (If cond thenFork Nothing) = print2 "if (%) {\n%\n}" (concat . map toPhp $ cond) (toPhp thenFork)
  toPhp (While cond body) = print2 "while (%) {\n%\n}" (toPhp cond) (toPhp body)
  toPhp (Class name Nothing Nothing body) = print2 "class % {\n%\n}" (toPhp name) (toPhp body)
  toPhp (Class name (Just extendsName) Nothing body) = print3 "class % extends % {\n%\n}" (toPhp name) (toPhp extendsName) (toPhp body)
  toPhp (Class name Nothing (Just implementsName) body) = print3 "class % implements % {\n%\n}" (toPhp name) (toPhp implementsName) (toPhp body)
  toPhp (Class name (Just extendsName) (Just implementsName) body) = print4 "class % extends % implements % {\n%\n}" (toPhp name) (toPhp extendsName) (toPhp implementsName) (toPhp body)
  toPhp (New name args) = print2 "new %(%)" (toPhp name) (intercalate "," . map toPhp $ args)

  toPhp (Variable (InstanceVar ('_':s)) ClassScope) = "private $" ++ s
  toPhp (Variable (StaticVar ('_':s)) ClassScope) = "private static $" ++ s
  toPhp (Variable (SimpleVar ('_':s)) ClassScope) = "private $" ++ s
  toPhp (Variable (InstanceVar s) ClassScope) = "public $" ++ s
  toPhp (Variable (StaticVar s) ClassScope) = "public static $" ++ s
  toPhp (Variable (SimpleVar s) ClassScope) = "public $" ++ s
  toPhp (Variable x _) = toPhp x
  toPhp (WithNewLine s) = (toPhp s) ++ "\n"
  toPhp (HashLookup h k) = print2 "%[%]" (toPhp h) (toPhp k)
  toPhp (FunctionTypeSignature var types)
      | simpleVarName var == "var" = "<EMPTYLINE>\n/** @var " ++ (showVar . head $ types) ++ " */"
      | otherwise = "<EMPTYLINE>\n/**\n" ++ (concat $ map showType types) ++ " */\n"
    where showType t = " * " ++ (toPhp t) ++ "\n"
          showVar (ArgumentType False n _) = n
          showVar (ArgumentType True n _) = n ++ "|null"

  toPhp (Constant (Variable (InstanceVar ('_':s)) ClassScope)) = "private const " ++ s
  toPhp (Constant (Variable (StaticVar ('_':s)) ClassScope)) = "private static const " ++ s
  toPhp (Constant (Variable (SimpleVar ('_':s)) ClassScope)) = "private const " ++ s
  toPhp (Constant (Variable (ClassVar ('_':s)) ClassScope)) = "private const " ++ s
  toPhp (Constant (Variable (InstanceVar s) ClassScope)) = "public const " ++ s
  toPhp (Constant (Variable (StaticVar s) ClassScope)) = "public static const " ++ s
  toPhp (Constant (Variable (SimpleVar s) ClassScope)) = "public const " ++ s
  toPhp (Constant (Variable (ClassVar s) ClassScope)) = "public const " ++ s
  toPhp (Constant (Variable (InstanceVar s) _)) = s
  toPhp (Constant (Variable (StaticVar s) _)) = "static::" ++ s
  toPhp (Constant (Variable (ClassVar s) _)) = s
  toPhp (Constant (Variable (SimpleVar s) _)) = s

  toPhp (HashTable nameValuePairs) = "[\n" ++ hashBody ++ "\n]"
    where kvtoPhp (name, val) = print2 "% => %" (toPhp name) (toPhp val)
          hashBody = intercalate ",\n" $ map kvtoPhp nameValuePairs

  toPhp (DestructuredHash vars) = toPhp $ HashTable (zip (map SaltyString vars) (map (\s -> Variable (SimpleVar s) GlobalScope) vars))
  toPhp (ArraySlice obj start Nothing) = print2 "array_slice(%, %)" (toPhp obj) (toPhp start)
  toPhp (ArraySlice obj (SaltyNumber start) (Just (SaltyNumber end))) = print3 "array_slice(%, %, %)" (toPhp obj) start newEnd
    where newEnd = show $ (read end :: Integer) - (read start :: Integer)
  toPhp (ArraySlice obj start (Just end)) = print4 "array_slice(%, %, % - %)" (toPhp obj) (toPhp start) (toPhp end) (toPhp start)
  toPhp (StringSlice obj start Nothing) = print2 "substr(%, %)" (toPhp obj) (toPhp start)
  toPhp (StringSlice obj (SaltyNumber start) (Just (SaltyNumber end))) = print3 "substr(%, %, %)" (toPhp obj) start newEnd
    where newEnd = show $ (read end :: Integer) - (read start :: Integer)
  toPhp (StringSlice obj start (Just end)) = print4 "substr(%, %, % - %)" (toPhp obj) (toPhp start) (toPhp end) (toPhp start)
  toPhp (StringIndex obj index) = print2 "substr(%, %, 1)" (toPhp obj) (toPhp index)
  toPhp (AttrAccess (Variable (ClassVar obj) _) attrName)
      | isConstant attrName = print2 "%::%" obj attrName
      | otherwise = print2 "%::$%" obj attrName

  toPhp (AttrAccess obj attrName) = print2 "%->%" (toPhp obj) attrName
  toPhp (MultiAssign vars (WithNewLine value)) = toPhp $ WithNewLine (MultiAssign vars value)
  toPhp (MultiAssign vars value@(Variable _ _)) = (intercalate "\n" . map (\(i,var) -> print3 "% = %[%]" (toPhp var) (toPhp value) (show i)) $ zip [0..] vars)
  toPhp (MultiAssign vars value@(FunctionCall _ _ _ _)) = initResult ++ "\n" ++ multiAssign
      where initResult = "$result = " ++ (toPhp value)
            multiAssign = (intercalate "\n" . map (\(i,var) -> print2 "% = $result[%]" (toPhp var) (show i)) $ zip [0..] vars)
  toPhp (MultiAssign vars value) = (intercalate "\n" . map (\var -> print2 "% = %" (toPhp var) (toPhp value)) $ vars)
  -- toPhp (AttrAccess (Variable (InstanceVar obj) _) attrName) = print2 "$this->%->%" obj attrName
  -- toPhp (AttrAccess (Variable (StaticVar obj) _) attrName) = print2 "static::$%->%" obj attrName
  toPhp (Array salties@((Array _):rest)) = "[\n" ++ (intercalate ",\n" . map toPhp $ salties) ++ ",\n]"
  toPhp (Array salties) = "[" ++ (intercalate ", " . map toPhp $ salties) ++ "]"
  toPhp (WhereClause salty) = toPhp salty
  toPhp (Guard cond outcome) = print2 "if (%) {\n%\n}" (concat . map toPhp $ cond) (addReturnToArray outcome)
  toPhp (SaltyGuard Nothing ((Guard cond outcome):[])) = print2 "if (%) {\n%\n}" (concat . map toPhp $ cond) (addReturnToArray outcome)
  toPhp (SaltyGuard Nothing guards) = initGuards ++ lastGuard
    where initGuards = intercalate " else" . map toPhp . init $ guards
          lastGuard = case (last guards) of
                           (Guard [(SaltyString "otherwise")] outcome) -> " else {\n" ++ (addReturnToArray outcome) ++  "\n}"
                           _ -> " else" ++ (toPhp (last guards))

  toPhp (SaltyGuard (Just val) guards) = print2 "switch (%) {\n%\n}" (toPhp val) guardsToPhp
    where guardsToPhp = concat . map phpFunc $ guards
          phpFunc (Guard cond outcome) = print2 "case %:\n    %\n" (toPhp_ cond) (join "\n" . map toPhp $ outcome)
          toPhp_ [(SaltyString "otherwise")] = "default"
          toPhp_ cond_ = (join "\n" . map toPhp $ cond_)

  toPhp (SaltyBool TRUE) = "true"
  toPhp (SaltyBool FALSE) = "false"
  toPhp SaltyNull = "null"
  toPhp (SaltyMagicConstant c) = toPhp c
  toPhp (Keyword (KwPreceding "const" (Operation (Constant (Variable var _)) op right))) = "const " ++ (toPhp (Operation (PurePhp (simpleVarName var)) op right))
  toPhp (Keyword (KwPreceding "const" (Constant (Variable var _)))) = "const " ++ (simpleVarName var)
  toPhp (Keyword (KwPreceding "const" (Variable var _))) = "const " ++ (simpleVarName var)

  toPhp (Keyword (KwPreceding str salty)) = str ++ " " ++ (toPhp salty)
  toPhp (Keyword (KwSimple str)) = str
  toPhp (Range (SaltyNumber l) (SaltyNumber r)) = show $ [left..right]
      where left = read l :: Integer
            right = read r :: Integer
  toPhp (Range l r) = "a range: (" ++ (toPhp l) ++ ".." ++ (toPhp r) ++ ")"
  toPhp (Keyword x) = "keyword not implemented yet: " ++ (show x)
  toPhp (ParseError x) = x ++ "\n"
  toPhp SaltySpace = " "
  toPhp x = "not implemented yet: " ++ (show x)


addReturn :: Salty -> String
addReturn x@(ReturnStatement _) = toPhp x
addReturn x@(Operation var@(Variable _ _) Equals h@(HigherOrderFunctionCall obj callName func accVar)) = addReturn (HigherOrderFunctionCall obj callName func (varName var))
addReturn x@(Operation var@(Variable _ _) Equals (WithNewLine(h@(HigherOrderFunctionCall obj callName func accVar)))) = addReturn (HigherOrderFunctionCall obj callName func (varName var))
addReturn x@(Operation _ Equals _) = toPhp x
addReturn x@(Operation _ ArrayPush _) = toPhp x
addReturn x@(Operation _ PlusEquals _) = toPhp x
addReturn x@(Operation _ MinusEquals _) = toPhp x
addReturn x@(Operation _ MultiplyEquals _) = toPhp x
addReturn x@(Operation _ DivideEquals _) = toPhp x
addReturn x@(Operation left OrEquals _) = (toPhp x) ++ "\nreturn " ++ (toPhp left) ++ ";"
addReturn x@(Operation _ _ _) = "return " ++ (toPhp x)
addReturn (If cond thenFork (Just elseFork)) = print3 "if (%) {\n%\n} else {\n%\n}" (concat . map toPhp $ cond) (addReturn thenFork) (addReturn elseFork)
addReturn (If cond thenFork Nothing) = print2 "if (%) {\n%\n}" (concat . map toPhp $ cond) (addReturn thenFork)
addReturn x@(Braces []) = toPhp x
addReturn (Braces s) = (concat . map toPhp . init $ s) ++ "\n" ++ (addReturn . last $ s)
addReturn (Variable name scope) = "return " ++ (toPhp name)
addReturn (WithNewLine x) = (addReturn x) ++ "\n"
addReturn (Parens [x@(If _ _ _)]) = addReturn x
addReturn p@(Parens x) = "return " ++ (toPhp p)
addReturn f@(FunctionCall o n a b) = "return " ++ (toPhp f)
addReturn h@(HashTable kv) = "return " ++ (toPhp h)
addReturn a@(Array xs) = "return " ++ (toPhp a)
addReturn f@(HigherOrderFunctionCall _ Each _ _) = toPhp f
addReturn f@(HigherOrderFunctionCall _ _ _ accVar) = (toPhp f) ++ "\nreturn " ++ accVar
addReturn a@(AttrAccess _ _) = "return " ++ (toPhp a)
addReturn x@(SaltyNumber _) = "return " ++ (toPhp x)
addReturn x@(SaltyString _) = "return " ++ (toPhp x)
addReturn x@(SaltyBool _) = "return " ++ (toPhp x)
addReturn x@(Negate _) = "return " ++ (toPhp x)
addReturn x@(SaltyGuard (Just val) guards) = toPhp (SaltyGuard (Just val) newGuards)
  where newGuards = map addReturn_ guards
        addReturn_ (Guard cond outcome) = Guard cond ((init outcome) ++ [ReturnStatementForAddReturn (last outcome)])
addReturn x = toPhp x
