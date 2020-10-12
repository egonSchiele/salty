module Types where

import Text.Printf
import Data.List (intercalate)

data VariableName = InstanceVar String | ClassVar String | SimpleVar String deriving (Show)

varName :: VariableName -> String
varName x = case x of
    InstanceVar s -> s
    ClassVar s -> s
    SimpleVar s -> s

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
             | TypeDefinition { -- e.g. foo :: String, String -> Num. tTypes would be ["String", "String", "Num"]
               tName :: String,
               tTypes :: [String]
             }
             | FeatureFlag String
             | ExistenceCheck { -- e.g. "hash ? key" gets converted to "isset($hash[$key])"
               eHash :: VariableName,
               eKey :: VariableName
             }
             | HashGet { -- e.g. "hash > key" -> "$hash[$key]"
               eHash :: VariableName,
               eKey :: VariableName
             }
             | FMapCall {
               fmapObject :: Salty,
               fmapFunction :: FunctionBody
             }
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
  toPhp (AmpersandFunction fName) = printf "$%s($var);" (toPhp fName)

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
  toPhp (FunctionCall (Just (SimpleVar obj)) funcName args) = printf "$%s->%s(%s)" obj (varName funcName) (intercalate ", " args)
  toPhp (FunctionCall (Just (InstanceVar obj)) funcName args) = printf "$this->%s->%s(%s)" obj (varName funcName) (intercalate ", " args)
  toPhp (FunctionCall (Just (ClassVar obj)) funcName args) = printf "static::%s->%s(%s)" obj (varName funcName) (intercalate ", " args)

  toPhp x = "not implemented yet: " ++ (show x)


    --             | FunctionCall { -- e.g. obj.foo / obj.foo(1) / foo(1, 2)
               -- fObject :: Maybe VariableName,
               -- fCallName :: VariableName,
               -- fCallArguments :: [String]
             -- }

