module Types where

data Visibility = Public | Private deriving (Show)

data Boolean = TRUE | FALSE deriving (Show)

data VariableName =
    InstanceVar String -- e.g. $this->foo
  | StaticVar String -- e.g. self::foo or static::foo
  | ClassVar String -- e.g.Blocklist::foo
  | SimpleVar String -- e.g. foo
  deriving (Show)

data PhpKeyword =   KwUse VariableName (Maybe VariableName)
                  | KwThrow Salty
                  | KwRequire Salty
                  | KwRequireOnce Salty
                  | KwConst Salty
                  | KwPublic Salty
                  | KwPrivate Salty
                  | KwProtected Salty
                  | KwStatic Salty
                  | KwEcho Salty
                  | KwBreak
                  | KwUndefined
                  | KwNamespace Salty deriving (Show)

data MagicConstant =   MCLINE
                     | MCFILE
                     | MCDIR
                     | MCFUNCTION
                     | MCCLASS
                     | MCTRAIT
                     | MCMETHOD
                     | MCNAMESPACE
                     deriving (Show)

-- function args
data Argument = Argument {
                  argType :: Maybe ArgumentType,
                  argName :: ArgumentName,
                  argDefault :: Maybe String
                } deriving (Show)

data ArgumentName = ArgumentName {
                      argNameName :: String,
                      argNameByReference :: Bool
                    } deriving (Show)

data ArgumentType = ArgumentType {
                      aOptional :: Bool,
                      aType :: String,
                      aReturnArg :: Bool
                    } deriving (Show)

data HigherOrderFunction = Each | Map | Select | Any | All deriving (Show)

data Operator = Add |
  Subtract |
  Divide |
  Multiply |
  Modulo |
  Equals |
  NotEquals |
  PlusEquals |
  MinusEquals |
  OrEquals |
  DivideEquals |
  MultiplyEquals |
  OrOr |
  AndAnd |
  NullCoalesce |
  PlusPlus |
  ArrayMerge |
  ArrayDiff |
  In |
  KeyIn |
  InstanceOf |
  EqualsEquals |
  LessThan |
  LessThanOrEqualTo |
  GreaterThan |
  GreaterThanOrEqualTo |
  Spaceship |
  ArrayPush deriving (Show)

data BuiltInFunction = VarDumpShort deriving (Show)

data Scope = GlobalScope | ClassScope | FunctionScope deriving (Show, Eq)


data Salty = Operation { -- e.g. a = 1 / a += 1 / a ||= 0
               oLeft :: Salty,
               oOperationType :: Operator,
               oRight :: Salty
             }
             | Function {
               fName :: VariableName,
               fArguments :: [Argument],
               fBody :: [Salty],
               fVisibility :: Visibility,
               fScope :: Scope
             }
             | FunctionTypeSignature {
               fName :: VariableName,
               fTypes :: [ArgumentType]
             }
             | SaltyNumber String
             | SaltyString String
             | AttrAccess { -- e.g. obj.foo
                 attrObject :: Salty,
                 attrAttr :: String
             }
             | FunctionCall { -- e.g. obj.foo(1) / foo(1, 2)
                 fObject :: Maybe Salty,
                 fCallName :: Either BuiltInFunction VariableName,
                 fCallArguments :: [Salty]
             }
             | HigherOrderFunctionCall { -- higher order function call. I'm adding support for a few functions like map/filter/each
               hoObject :: Salty,
               hoCallName :: HigherOrderFunction,
               hoFunction :: Salty, -- a lambda function
               hoAccVar :: String
             }
             | LambdaFunction { -- \a b -> a + b
               lArguments :: [String],
               lBody :: Salty
             }
             | If {
               condition :: [Salty],
               thenPath :: Salty,
               elsePath :: Maybe Salty
             }
             | While {
               wCondition :: Salty,
               wBody :: Salty
             }
             | New {
               fClassName :: VariableName,
               fConstructorArgs :: [Salty]
             }
             | Class {
               cName :: VariableName,
               cExtendsName :: Maybe VariableName,
               cImplementsName :: Maybe VariableName,
               wBody :: Salty
             }
             | HashLookup {
                 hHash :: Salty,
                 hKey :: Salty
             }
             | HashTable [(Salty, Salty)]
             | DestructuredHash [String]
             | ArraySlice {
                arObj :: Salty,
                arStart :: Salty,
                arEnd :: Maybe Salty
             }
             | StringIndex {
                striObj :: Salty,
                striIndex :: Salty
             }
             | StringSlice {
                strObj :: Salty,
                strStart :: Salty,
                strEnd :: Maybe Salty
             }
             | Range {
                rangeLeft :: Salty,
                rangeRight :: Salty
             }
             | MultiAssign {
               muVars :: [Salty],
               muValue :: Salty
             }
             | SaltyGuard (Maybe Salty) [Salty]
             | Guard {
                gCondition :: [Salty],
                gOutcome :: [Salty]
             }
             | Array [Salty]
             | ReturnStatement Salty
             | ReturnStatementForAddReturn Salty
             | Negate Salty
             | EmptyLine
             | WithNewLine Salty
             | Parens [Salty]
             | Braces [Salty]
             -- | Multi [Salty]
             | PurePhp String
             | PhpComment String
             | SaltyComment String
             | Salt
             | BackTrack Salty
             | Constant Salty
             | Variable VariableName Scope
             | SaltyBool Boolean
             | SaltyNull
             | SaltyMagicConstant MagicConstant
             | Keyword PhpKeyword
             | ParseError String
             | SaltyOptional Salty
             deriving (Show)

isSaltyComment :: Salty -> Bool
isSaltyComment (SaltyComment _) = True
isSaltyComment _ = False
