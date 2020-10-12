module Types where

import Text.Printf
import Data.List (intercalate)

data VariableName = InstanceVar String | ClassVar String | SimpleVar String deriving (Show)

simpleVarName :: VariableName -> String
simpleVarName x = case x of
    InstanceVar s -> s
    ClassVar s -> s
    SimpleVar s -> s

varName :: VariableName -> String
varName x = case x of
    InstanceVar str -> "$this->" ++ str
    ClassVar str -> "static::$" ++ str
    SimpleVar str -> "$" ++ str

data FunctionBody = OneLine Salty -- e.g. incr x := x + 1
                      | Block [Salty] -- fib x := do if x < 2 .... end
                      | LambdaFunction { -- \a b -> a + b
                        lArguments :: [String],
                        lBody :: FunctionBody
                      }
                      | AmpersandFunction VariableName -- &:@function (used for maps/each etc)
                      deriving (Show)

data AssignmentType = Equals | PlusEquals | MinusEquals | OrEquals deriving (Show)

-- function args
data Argument = Argument {
                  argType :: String,
                  argName :: String,
                  argDefault :: Maybe String
                } deriving (Show)

data HigherOrderCall = Each | Map | Select | Any | All deriving (Show)

data Salty = Assignment { -- e.g. a = 1 / a += 1 / a ||= 0
               aName :: VariableName,
               aAssignmentType :: AssignmentType,
               aValue :: Salty
             }
             | Function {
               fName :: VariableName,
               fArguments :: [Argument],
               fBody :: FunctionBody
             }
             | SaltyNumber String
             | SaltyString String
             | FunctionCall { -- e.g. obj.foo / obj.foo(1) / foo(1, 2)
               fObject :: Maybe VariableName,
               fCallName :: VariableName,
               fCallArguments :: [String]
             }
             | HigherOrderFunctionCall { -- higher order function call. I'm adding support for a few functions like map/filter/each
               hoObject :: VariableName,
               hoCallName :: HigherOrderCall,
               hoFunction :: FunctionBody  --  either lambda or ampersand function.
             }
             -- | TypeDefinition { -- e.g. foo :: String, String -> Num. tTypes would be ["String", "String", "Num"]
             --   tName :: String,
             --   tTypes :: [String]
             -- }
             -- | FeatureFlag String
             -- | ExistenceCheck { -- e.g. "hash ? key" gets converted to "isset($hash[$key])"
             --   eHash :: VariableName,
             --   eKey :: VariableName
             -- }
             | HashLookup { -- e.g. "hash > key" -> "$hash[$key]"
               eHash :: Either VariableName Salty,
               eKey :: VariableName
             }
             -- | FMapCall {
             --   fmapObject :: Salty,
             --   fmapFunction :: FunctionBody
             -- }
             deriving (Show)


class ConvertToPhp a where
    toPhp :: a -> String

instance ConvertToPhp VariableName where
  toPhp (InstanceVar s) = "$this->" ++ s
  toPhp (ClassVar s) = "self::" ++ s
  toPhp (SimpleVar s) = '$':s

instance ConvertToPhp FunctionBody where
  toPhp (OneLine s) = (toPhp s) ++ ";"
  toPhp (Block s) = (intercalate ";\n" $ map toPhp s) ++ ";"
  toPhp (LambdaFunction args body) = (unlines $ map (printf "$%s = null;\n") args) ++ toPhp body
  toPhp (AmpersandFunction (SimpleVar str)) = printf "%s($i)" str
  toPhp (AmpersandFunction (InstanceVar str)) = printf "$i->%s()" str

instance ConvertToPhp Argument where
  toPhp (Argument typ name (Just default_)) = printf "?%s $%s = %s" typ name default_
  toPhp (Argument typ name Nothing) = typ ++ " $" ++ name

instance ConvertToPhp Salty where
  toPhp (Assignment name Equals value) = (toPhp name) ++ " = " ++ (toPhp value)
  toPhp (Assignment name PlusEquals value) = printf "%s = %s + %s" n n (toPhp value)
    where n = toPhp name
  toPhp (Assignment name MinusEquals value) = printf "%s = %s - %s" n n (toPhp value)
    where n = toPhp name
  toPhp (Assignment name OrEquals value) = printf "%s = %s ?? %s" n n (toPhp value)
    where n = toPhp name

  toPhp (Function name args (LambdaFunction _ _)) = "lambda function body not allowed as method body " ++ (show name)
  toPhp (Function name args (AmpersandFunction _)) = "ampersand function body not allowed as method body " ++ (show name)
  toPhp (Function name args body) = printf "%s(%s) {\n%s\n}" funcName funcArgs funcBody
    where funcName = case name of
            InstanceVar str -> "function " ++ str
            ClassVar str -> "static function " ++ str
            SimpleVar str -> "function " ++ str
          funcArgs = intercalate ", " $ map toPhp args
          funcBody = case body of
              OneLine s -> (toPhp s) ++ ";"
              Block s -> (intercalate ";\n" $ map toPhp s) ++ ";"
              _ -> "invalid funcBody"

  toPhp (SaltyNumber s) = s
  toPhp (SaltyString s) = s

  toPhp (FunctionCall Nothing (SimpleVar str) args) = printf "%s(%s)" str (intercalate ", " args)
  toPhp (FunctionCall Nothing (InstanceVar str) args) = printf "$this->%s(%s)" str (intercalate ", " args)
  toPhp (FunctionCall Nothing (ClassVar str) args) = printf "static::%s(%s)" str (intercalate ", " args)
  toPhp (FunctionCall (Just (SimpleVar obj)) funcName args) = printf "$%s->%s(%s)" obj (simpleVarName funcName) (intercalate ", " args)
  toPhp (FunctionCall (Just (InstanceVar obj)) funcName args) = printf "$this->%s->%s(%s)" obj (simpleVarName funcName) (intercalate ", " args)
  toPhp (FunctionCall (Just (ClassVar obj)) funcName args) = printf "static::%s->%s(%s)" obj (simpleVarName funcName) (intercalate ", " args)

  toPhp (HigherOrderFunctionCall obj Each (LambdaFunction (loopVar:xs) body)) = printf "foreach (%s as $%s) {\n%s;\n}" (varName obj) loopVar (toPhp body)

  toPhp (HigherOrderFunctionCall obj Each af@(AmpersandFunction name)) = printf "foreach (%s as $i) {\n%s;\n}" (varName obj) (toPhp af)

  toPhp (HigherOrderFunctionCall obj Map (LambdaFunction (loopVar:accVar:xs) body)) = printf "$%s = [];\nforeach (%s as $%s) {\n$%s []= %s;\n}" accVar (varName obj) loopVar accVar (toPhp body)
  toPhp (HigherOrderFunctionCall obj Map af@(AmpersandFunction name)) = printf "$acc = [];\nforeach (%s as $i) {\n$acc []= %s;\n}" (varName obj) (toPhp af)

  toPhp (HigherOrderFunctionCall obj Select (LambdaFunction (loopVar:accVar:xs) body)) = printf "$%s = [];\nforeach (%s as $%s) {\nif(%s) {\n$%s []= %s;\n}\n}" accVar (varName obj) loopVar (toPhp body) accVar loopVar
  toPhp (HigherOrderFunctionCall obj Select af@(AmpersandFunction name)) = printf "$acc = [];\nforeach (%s as $i) {\nif(%s) {\n$acc []= $i;\n}\n}" (varName obj) (toPhp af)
  toPhp (HigherOrderFunctionCall obj Any (LambdaFunction (loopVar:xs) body)) = printf "$result = false;\nforeach (%s as $%s) {\nif(%s) {\n$result = true;\nbreak;\n}" (varName obj) loopVar (toPhp body)
  toPhp (HigherOrderFunctionCall obj Any af@(AmpersandFunction name)) = printf "$result = false;\nforeach (%s as $i) {\nif(%s) {\n$result = true;\nbreak;\n}" (varName obj) (toPhp af)
  toPhp (HigherOrderFunctionCall obj All (LambdaFunction (loopVar:xs) body)) = printf "$result = true;\nforeach (%s as $%s) {\nif(!%s) {\n$result = false;\nbreak;\n}" (varName obj) loopVar (toPhp body)
  toPhp (HigherOrderFunctionCall obj All af@(AmpersandFunction name)) = printf "$result = true;\nforeach (%s as $i) {\nif(!%s) {\n$result = false;\nbreak;\n}" (varName obj) (toPhp af)

  toPhp (HashLookup (Left var) key) = printf "%s[%s]" (varName var) (varName key)
  toPhp (HashLookup (Right hashLookup_) key) = printf "%s[%s]" (toPhp hashLookup_) (varName key)

  toPhp x = "not implemented yet: " ++ (show x)

             -- | HigherOrderFunctionCall { -- higher order function call. I'm adding support for a few functions like map/filter/each
             --   hoObject :: VariableName,
             --   hoCallName :: HigherOrderCall,
             --   hoFunction :: FunctionBody  --  either lambda or ampersand function.
             -- }
             -- | HashLookup { -- e.g. "hash > key" -> "$hash[$key]"
             --   eHash :: Either VariableName HashLookup,
             --   eKey :: VariableName
             -- }
