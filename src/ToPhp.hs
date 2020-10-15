module ToPhp where
import Types
import Print
import Data.List (intercalate)

class ConvertToPhp a where
    toPhp :: a -> String

instance ConvertToPhp VariableName where
  toPhp (InstanceVar s) = "$this->" ++ s
  toPhp (ClassVar s) = "self::$" ++ s
  toPhp (SimpleVar s) = '$':s

instance ConvertToPhp Argument where
  toPhp (Argument (Just typ) name (Just default_)) = print3 "?% $% = %" typ name default_
  toPhp (Argument (Just typ) name Nothing) = typ ++ " $" ++ name
  toPhp (Argument Nothing name (Just default_)) = print2 "$% = %" name default_
  toPhp (Argument Nothing name Nothing) = "$" ++ name

instance (ConvertToPhp a1, ConvertToPhp a2) => ConvertToPhp (Either a1 a2) where
  toPhp (Left a) = toPhp a
  toPhp (Right a) = toPhp a

instance ConvertToPhp Salty where
  toPhp (Operation left Equals right) = (toPhp left) ++ " = " ++ (toPhp right)
  toPhp (Operation left NotEquals right) = (toPhp left) ++ " != " ++ (toPhp right)
  toPhp (Operation left PlusEquals right) = print3 "% = % + %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left MinusEquals right) = print3 "% = % - %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left MultiplyEquals right) = print3 "% = % * %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left DivideEquals right) = print3 "% = % / %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left OrEquals right) = print3 "% = % ?? %" (toPhp left) (toPhp left) (toPhp right)
  toPhp (Operation left Add right) = print2 "% + %" (toPhp left) (toPhp right)
  toPhp (Operation left Subtract right) = print2 "% - %" (toPhp left) (toPhp right)
  toPhp (Operation left Divide right) = print2 "% / %" (toPhp left) (toPhp right)
  toPhp (Operation left Multiply right) = print2 "% * %" (toPhp left) (toPhp right)
  toPhp (Operation left OrOr right) = print2 "% || %" (toPhp left) (toPhp right)
  toPhp (Operation left AndAnd right) = print2 "% && %" (toPhp left) (toPhp right)
  toPhp (Operation left LessThan right) = print2 "% < %" (toPhp left) (toPhp right)
  toPhp (Operation left LessThanOrEqualTo right) = print2 "% <= %" (toPhp left) (toPhp right)
  toPhp (Operation left GreaterThan right) = print2 "% > %" (toPhp left) (toPhp right)
  toPhp (Operation left GreaterThanOrEqualTo right) = print2 "% >= %" (toPhp left) (toPhp right)

  toPhp (Function name args body) = print3 "%(%) {\n%\n}" funcName funcArgs (concat $ map toPhp body)
    where funcName = case name of
            InstanceVar str -> "function " ++ str
            ClassVar str -> "static function " ++ str
            SimpleVar str -> "function " ++ str
          funcArgs = intercalate ", " $ map toPhp args

  toPhp (SaltyNumber s) = s
  toPhp (SaltyString s) = "\"" ++ s ++ "\""

  toPhp (FunctionCall Nothing (SimpleVar str) args) = print2 "%(%)" str (intercalate ", " args)
  toPhp (FunctionCall Nothing (InstanceVar str) args) = print2 "$this->%(%)" str (intercalate ", " args)
  toPhp (FunctionCall Nothing (ClassVar str) args) = print2 "static::%(%)" str (intercalate ", " args)
  toPhp (FunctionCall (Just (Variable (SimpleVar obj))) funcName args) = print3 "$%->%(%)" obj (simpleVarName funcName) (intercalate ", " args)
  toPhp (FunctionCall (Just (Variable (InstanceVar obj))) funcName args) = print3 "$this->%->%(%)" obj (simpleVarName funcName) (intercalate ", " args)
  toPhp (FunctionCall (Just (Variable (ClassVar obj))) funcName args) = print3 "static::%->%(%)" obj (simpleVarName funcName) (intercalate ", " args)

  toPhp (LambdaFunction [] body) =  toPhp body
  toPhp (LambdaFunction (a:args) body) = ("$" ++ a ++ " = null;\n") ++ (toPhp $ LambdaFunction args body)

  -- each
  toPhp (HigherOrderFunctionCall obj Each (LambdaFunction (loopVar:xs) body)) =
                print3 "foreach (% as $%) {\n%;\n}" (varName obj) loopVar (toPhp body)

  -- toPhp (HigherOrderFunctionCall obj Each af@(AmpersandFunction name)) =
  --               print3 "foreach (% as $i) {\n%;\n}" (varName obj) (toPhp af)

  -- map
  toPhp (HigherOrderFunctionCall obj Map (LambdaFunction (loopVar:accVar:[]) body)) =
                print5 "$% = [];\nforeach (% as $%) {\n$% []= %;\n}" accVar (varName obj) loopVar accVar (toPhp body)

  toPhp (HigherOrderFunctionCall obj Map (LambdaFunction (loopVar:[]) body)) =
                print5 "$% = [];\nforeach (% as $%) {\n$% []= %;\n}" accVar (varName obj) loopVar accVar (toPhp body)
                  where accVar = "result"

  -- toPhp (HigherOrderFunctionCall obj Map af@(AmpersandFunction name)) =
  --               print3 "$acc = [];\nforeach (% as $i) {\n$acc []= %;\n}" (varName obj) (toPhp af)

  -- select
  toPhp (HigherOrderFunctionCall obj Select (LambdaFunction (loopVar:accVar:[]) body)) =
                print6 "$% = [];\nforeach (% as $%) {\nif(%) {\n$% []= %;\n}\n}" accVar (varName obj) loopVar (toPhp body) accVar loopVar
  toPhp (HigherOrderFunctionCall obj Select (LambdaFunction (loopVar:[]) body)) =
                print6 "$% = [];\nforeach (% as $%) {\nif(%) {\n$% []= %;\n}\n}" accVar (varName obj) loopVar (toPhp body) accVar loopVar
                        where accVar = "result"
  -- toPhp (HigherOrderFunctionCall obj Select af@(AmpersandFunction name)) =
  --               print3 "$acc = [];\nforeach (% as $i) {\nif(%) {\n$acc []= $i;\n}\n}" (varName obj) (toPhp af)

  -- any
  toPhp (HigherOrderFunctionCall obj Any (LambdaFunction (loopVar:xs) body)) =
                print3 "$result = false;\nforeach (% as $%) {\nif(%) {\n$result = true;\nbreak;\n}\n}" (varName obj) loopVar (toPhp body)
  -- toPhp (HigherOrderFunctionCall obj Any af@(AmpersandFunction name)) =
  --               print3 "$result = false;\nforeach (% as $i) {\nif(%) {\n$result = true;\nbreak;\n}\n}" (varName obj) (toPhp af)

  -- all
  toPhp (HigherOrderFunctionCall obj All (LambdaFunction (loopVar:xs) body)) =
                print3 "$result = true;\nforeach (% as $%) {\nif(!%) {\n$result = false;\nbreak;\n}\n}" (varName obj) loopVar (toPhp body)
  -- toPhp (HigherOrderFunctionCall obj All af@(AmpersandFunction name)) =
  --               print3 "$result = true;\nforeach (% as $i) {\nif(!%) {\n$result = false;\nbreak;\n}\n}" (varName obj) (toPhp af)

  toPhp Salt = "I'm salty"
  toPhp (ReturnStatement s) = "return " ++ (toPhp s) ++ ";"
  toPhp (Parens s) = "(" ++ (concat $ map toPhp s) ++ ")"
  toPhp (Braces s) = concat $ map toPhp s
  toPhp (PhpLine line) = line
  toPhp (PhpComment str) = "// " ++ str
  toPhp (SaltyComment str) = ""
  toPhp (Negate s) = "!" ++ (toPhp s)
  toPhp EmptyLine = ""
  toPhp (BackTrack s) = toPhp s

  toPhp (If cond thenFork (Just elseFork)) = print3 "if (%) {\n%\n} else {\n%\n}" (toPhp cond) (toPhp thenFork) (toPhp elseFork)
  toPhp (If cond thenFork Nothing) = print2 "if (%) {\n%\n}" (toPhp cond) (toPhp thenFork)

  toPhp (Variable x) = toPhp x
  toPhp (WithNewLine s) = (toPhp s) ++ "\n"

  toPhp x = "not implemented yet: " ++ (show x)