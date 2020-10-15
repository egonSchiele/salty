module Types where

data VariableName =
    InstanceVar String -- e.g. $this->foo
  | StaticVar String -- e.g. self::foo or static::foo
  | ClassVar String -- e.g.Blocklist::foo
  | SimpleVar String -- e.g. foo
  deriving (Show)

-- function args
data Argument = Argument {
                  argType :: Maybe String,
                  argName :: String,
                  argDefault :: Maybe String
                } deriving (Show)

argWithDefaults name = Argument Nothing name Nothing

data HigherOrderFunction = Each | Map | Select | Any | All deriving (Show)

data Operator = Add |
  Subtract |
  Divide |
  Multiply |
  Equals |
  NotEquals |
  PlusEquals |
  MinusEquals |
  OrEquals |
  DivideEquals |
  MultiplyEquals |
  OrOr |
  AndAnd |
  EqualsEquals |
  LessThan |
  LessThanOrEqualTo |
  GreaterThan |
  GreaterThanOrEqualTo deriving (Show)

data BuiltInFunction = VarDumpShort deriving (Show)

data Salty = Operation { -- e.g. a = 1 / a += 1 / a ||= 0
               oLeft :: Salty,
               oOperationType :: Operator,
               oRight :: Salty
             }
             | Function {
               fName :: VariableName,
               fArguments :: [Argument],
               fBody :: [Salty]
             }
             | SaltyNumber String
             | SaltyString String
             | FunctionCall { -- e.g. obj.foo / obj.foo(1) / foo(1, 2)
                 fObject :: Maybe Salty,
                 fCallName :: Either BuiltInFunction VariableName,
                 fCallArguments :: [Salty]
             }
             | HigherOrderFunctionCall { -- higher order function call. I'm adding support for a few functions like map/filter/each
               hoObject :: Salty,
               hoCallName :: HigherOrderFunction,
               hoFunction :: Salty  --  either lambda or ampersand function.
             }
             | LambdaFunction { -- \a b -> a + b
               lArguments :: [String],
               lBody :: Salty
             }
             | If {
               condition :: Salty,
               thenPath :: Salty,
               elsePath :: Maybe Salty
             }
             | While {
               wCondition :: Salty,
               wBody :: Salty
             }
             | HashLookup {
                 hHash :: Salty,
                 hKey :: Salty
             }
             | ReturnStatement Salty
             | Negate Salty
             | EmptyLine
             | WithNewLine Salty
             | Parens [Salty]
             | Braces [Salty]
             | PhpLine String
             | PhpComment String
             | SaltyComment String
             | Salt
             | BackTrack Salty
             | Variable VariableName
             deriving (Show)

isSaltyComment :: Salty -> Bool
isSaltyComment (SaltyComment _) = True
isSaltyComment _ = False
