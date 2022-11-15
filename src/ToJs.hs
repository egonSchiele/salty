module ToJs where
import Types
import Print
import Data.List (intercalate)
import Utils (isConstant, join, stripNewline)

initToJs body = join "\n" $ map toJs (init body)
stripNewlineToJs (WithNewLine salty) = toJs salty
stripNewlineToJs salty = toJs salty

_maybe f Nothing = ""
_maybe f (Just x) = f x

isEmptyLine EmptyLine = True
isEmptyLine _ = False

toReact :: Salty -> String
toReact (WithNewLine s) = toReact s
toReact (Braces s) = join "\n" . map toReact . filter (not . isEmptyLine) $ s
toReact (SaltyString str) = str
toReact f@(FunctionCall _ (Right (SimpleVar "new")) _ _) = toJs f
toReact f@(Function (SimpleVar "tag") [Argument _ (ArgumentName name _) _] body _ _) = toJs f
toReact EmptyLine = ""
toReact x@(PurePhp _) = toJs x
toReact x = "{" ++ (toJs x) ++ "}"

getAccVar var
  | var == "$result" = ""
  | otherwise = var ++ " = "

simpleVarName :: VariableName -> String
simpleVarName x = case x of
    InstanceVar s -> s
    StaticVar s -> s
    ClassVar s -> s
    SimpleVar s -> s

tagName :: VariableName -> String
tagName x
  | simpleVarName x == "fragment" = ""
  | otherwise = simpleVarName x

formatLoopVars [] = "x"
formatLoopVars (x:[]) = x
formatLoopVars (x:y:[]) = print2 "%, %" x y

addReturnToArray :: [Salty] -> String
addReturnToArray [] = ""
addReturnToArray (x:[]) = addReturn x
addReturnToArray x = ((join "\n") . map toJs . init $ x) ++ "\n" ++ (addReturn . last $ x)

varName :: Salty -> String
varName (Variable name scope) = toJs name
varName x = "this shouldnt be in varName: " ++ (show x)

varNameToFunc (InstanceVar s) = "this." ++ s
varNameToFunc (StaticVar s) = "this.state." ++ s
varNameToFunc (ClassVar s) = "error classvar as funcname"
varNameToFunc (SimpleVar s) = s

isPrivate Private = True
isPrivate _ = False

class ConvertToJs a where
    toJs :: a -> String

instance ConvertToJs VariableName where
  toJs (InstanceVar s) = "this." ++ s
  toJs (StaticVar s) = "this.state." ++ s
  toJs (ClassVar s) = s
  toJs (SimpleVar s) = s

instance ConvertToJs ArgumentType where
  toJs (ArgumentType True typ False) = "@param " ++ typ ++ "|null"
  toJs (ArgumentType False typ False) = "@param " ++ typ
  toJs (ArgumentType True typ True) = "@return " ++ typ ++ "|null"
  toJs (ArgumentType False typ True) = "@return " ++ typ

instance ConvertToJs Visibility where
  toJs Public = "public"
  toJs Private = "private"

instance ConvertToJs MagicConstant where
  toJs _ = "js doesn't have magic constants"

instance ConvertToJs ArgumentName where
  toJs (ArgumentName name _)
    | (take 3 name == "...") = "..." ++ (drop 3 name)
    | otherwise = name
instance ConvertToJs Argument where
  toJs (Argument (Just (ArgumentType False typ _)) name (Just default_)) = print3 "?% % = %" typ (toJs name) default_
  toJs (Argument (Just (ArgumentType True typ _)) name (Just default_)) = print3 "?% % = %" typ (toJs name) default_
  toJs (Argument (Just (ArgumentType False typ _)) name Nothing) = print2 "%: %" (toJs name) typ
  toJs (Argument (Just (ArgumentType True typ _)) name Nothing) = print2 "?% % = null" typ (toJs name)
  toJs (Argument Nothing name (Just default_)) = print2 "% = %" (toJs name) default_
  toJs (Argument Nothing name Nothing) = toJs name

instance (ConvertToJs a1, ConvertToJs a2) => ConvertToJs (Either a1 a2) where
  toJs (Left a) = toJs a
  toJs (Right a) = toJs a

instance ConvertToJs Salty where
  toJs (Operation x op (WithNewLine y)) = (toJs $ Operation x op y) ++ "\n"
  toJs (Operation (Variable (StaticVar name) _) Equals right) = print2 "this.setState({\n  %: %\n})" name (toJs right)
  toJs (Operation (HashLookup (Variable (StaticVar name) _) key) Equals right) = line1 ++ line2 ++ line3
      where line1 = print2 "let % = this.state.%\n" name name
            line2 = print3 "%[%] = %\n" name (toJs key) (toJs right)
            line3 = print2 "this.setState({\n  %: %\n})" name name

  toJs (Operation (AttrAccess (HashLookup (Variable (StaticVar name) _) key) attr) Equals right) = line1 ++ line2 ++ line3
      where line1 = print2 "let % = this.state.%\n" name name
            line2 = print4 "%[%].% = %\n" name (toJs key) attr (toJs right)
            line3 = print2 "this.setState({\n  %: %\n})" name name

  toJs (Operation (AttrAccess (Variable (StaticVar name) _) attr) Equals right) = line1 ++ line2 ++ line3
      where line1 = print2 "let % = Object.assign({}, this.state.%)\n" name name
            line2 = print3 "%.% = %\n" name attr (toJs right)
            line3 = print2 "this.setState({\n  %: %\n})" name name

  toJs (Operation x@(Variable _ _) Equals (HigherOrderFunctionCall obj callName func accVar)) = toJs $ HigherOrderFunctionCall obj callName func (varName x)
  toJs (Operation x@(Variable _ _) Equals (FunctionCall (Just (HigherOrderFunctionCall obj callName func accVar)) fName args block)) = toJs $ FunctionCall (Just $ HigherOrderFunctionCall obj callName func (varName x)) fName args block

  -- this is a hack -- it's the same as the statement above just w the WithNewLine added.
  -- toJs (Operation x@(Variable _ _) Equals (WithNewLine (HigherOrderFunctionCall obj callName func accVar))) = toJs $ HigherOrderFunctionCall obj callName func (varName x)

  -- currently hacking a semicolon at the end b/c otherwise my addSemicolons function won't because there's a colon (:) in the line.
  toJs (Operation left Equals (If cond thenPath_ (Just elsePath_))) = print4 "% = % ? % : %;" (toJs left) (concat . map toJs $ cond) (toJs thenPath_) (toJs elsePath_)
  toJs (Operation left Equals (If cond thenPath_ Nothing)) = print3 "% = null;\nif (%) {\n%\n}" (toJs left) (concat . map toJs $ cond) (toJs (Operation left Equals thenPath_))
  toJs (Operation (ArraySlice obj start Nothing) Equals arr) = print4 "%.splice(%, %.length, %)" (toJs obj) (toJs start) (toJs obj) (toJs arr)
  toJs (Operation (ArraySlice obj (SaltyNumber start) (Just (SaltyNumber end))) Equals arr) = print4 "%.splice(%, %, %)" (toJs obj) start newEnd (toJs arr)
    where newEnd = show $ (read end :: Integer) - (read start :: Integer)
  toJs (Operation (ArraySlice obj start (Just end)) Equals arr) = print5 "%.splice(%, % - %, %)" (toJs obj) (toJs start) (toJs end) (toJs start) (toJs arr)
  toJs (Operation left Equals right) = (toJs left) ++ " = " ++ (toJs right)
  toJs (Operation left NotEquals right) = (toJs left) ++ " !== " ++ (toJs right)
  -- toJs (Operation (Variable (StaticVar name) _) PlusEquals right) = print3 "this.setState({\n  %: % + %\n})" name name (toJs right)
  toJs (Operation left PlusEquals right) = print3 "% = % + %" (toJs left) (toJs left) (toJs right)
  toJs (Operation left MinusEquals right) = print3 "% = % - %" (toJs left) (toJs left) (toJs right)
  toJs (Operation left MultiplyEquals right) = print3 "% = % * %" (toJs left) (toJs left) (toJs right)
  toJs (Operation left DivideEquals right) = print3 "% = % / %" (toJs left) (toJs left) (toJs right)
  toJs (Operation left OrEquals right) = print3 "% = % ?? %" (toJs left) (toJs left) (toJs right)
  toJs (Operation (Variable (StaticVar name) _) ArrayPush right) = line1 ++ line2 ++ line3
      where line1 = print2 "let % = this.state.%.slice()\n" name name
            line2 = print2 "%.push(%)\n" name (toJs right)
            line3 = print2 "this.setState({\n  %: %\n})" name name
  toJs (Operation left ArrayPush right) = print2 "%.push(%)" (toJs left) (toJs right)
  toJs (Operation left Add right) = print2 "% + %" (toJs left) (toJs right)
  toJs (Operation left Subtract right) = print2 "% - %" (toJs left) (toJs right)
  toJs (Operation left Divide right) = print2 "% / %" (toJs left) (toJs right)
  toJs (Operation left Modulo right) = (toJs left) ++ " % " ++ (toJs right)
  toJs (Operation left Multiply right) = print2 "% * %" (toJs left) (toJs right)
  toJs (Operation left OrOr right) = print2 "% || %" (toJs left) (toJs right)
  toJs (Operation left AndAnd right) = print2 "% && %" (toJs left) (toJs right)
  toJs (Operation left NullCoalesce right) = print2 "% ?? %" (toJs left) (toJs right)
  toJs (Operation left PlusPlus right) = print2 "% + %" (toJs left) (toJs right)
  toJs (Operation left ArrayMerge right) = print2 "Object.assign(%, %)" (toJs left) (toJs right)
  toJs (Operation left ArrayDiff right) = print2 "array_diff(%, %)" (toJs left) (toJs right)
  toJs (Operation left In (Range start end)) = print4 "% >= % && % <= %" (toJs left) (toJs start) (toJs left) (toJs end)
  toJs (Operation left In (Parens [Range start end])) = print4 "% >= % && % <= %" (toJs left) (toJs start) (toJs left) (toJs end)
  toJs (Operation left In right) = print2 "%.includes(%)" (toJs right) (toJs left)
  toJs (Operation left KeyIn right) = print2 "%.hasOwnProperty(%)" (toJs right) (toJs left)
  toJs (Operation left InstanceOf right) = print2 "% instanceof %" (toJs left) (toJs right)
  toJs (Operation left EqualsEquals right) = print2 "% === %" (toJs left) (toJs right)
  toJs (Operation left LessThan right) = print2 "% < %" (toJs left) (toJs right)
  toJs (Operation left LessThanOrEqualTo right) = print2 "% <= %" (toJs left) (toJs right)
  toJs (Operation left GreaterThan right) = print2 "% > %" (toJs left) (toJs right)
  toJs (Operation left GreaterThanOrEqualTo right) = print2 "% >= %" (toJs left) (toJs right)
  toJs (Operation left Spaceship right) = print2 "% <=> %" (toJs left) (toJs right)

  toJs (Function name args body visibility scope)
    | scope == ClassScope = print3 "%(%) {\n%\n}\n" funcName funcArgs (funcBody body)
    | otherwise = print3 "const % = (%) => {\n%\n}\n" funcName funcArgs (funcBody body)
    where funcName = funcVisibility ++ (simpleVarName name)
          -- the parser strips out the leading '_' but we need to put it back for JS
          funcVisibility = if isPrivate visibility then "_" else ""
          funcArgs = intercalate ", " $ map toJs args
          funcBody b = case b of
                          [] -> ""
                          [Braces []] -> ""
                          [Braces salties] -> print2 "%\n%" ((join "\n") . map toJs . init $ salties) (addReturn . last $ salties)
                          x -> print2 "%\n%" ((join "\n") . map toJs . init $ x) (addReturn . last $ x)

  toJs (SaltyNumber s) = s
  toJs (SaltyString s) = "\"" ++ s ++ "\""

  -- functions called without an object (bare)
  toJs (FunctionCall Nothing (Right var) args Nothing) = print2 "%(%)" (varNameToFunc var) (intercalate ", " . map toJs $ args)

  toJs (FunctionCall Nothing (Right var) args (Just block@(LambdaFunction _ _))) = print2 "%(%)" (varNameToFunc var) args_
    where args_ = join ", " $ (map toJs args) ++ [(addReturn block)]

  -- builtin bare functions
  toJs (FunctionCall Nothing (Left VarDumpShort) args _) = "console.log(" ++ (intercalate ", " . map toJs $ args) ++ ")"

  -- functions called on higher order functions
  -- toJs (FunctionCall (Just hof@(HigherOrderFunctionCall _ _ _ accVar)) (Right funcName) args block) = print3 "%\n% = %" (toJs hof) accVar (toJs (FunctionCall (Just (PurePhp accVar)) (Right funcName) args block))
  -- builtin functions on an obj
  toJs (FunctionCall (Just obj) (Right (SimpleVar "uniq")) [] _) = print2 "let % = [...new Set(%)]" (toJs obj) (toJs obj)
  toJs (FunctionCall (Just obj) (Right (SimpleVar "sub")) [search, replace] _) = print3 "%.replace(%, %)" (toJs obj) (toJs search) (toJs replace)
  toJs (FunctionCall (Just (Variable vName _)) (Right (SimpleVar "new")) [] Nothing) = "<" ++ (tagName vName) ++ " />"
  toJs (FunctionCall (Just (Variable vName _)) (Right (SimpleVar "new")) [] (Just block)) = print3 "<%>%</%>" (tagName vName) (toReact block) (tagName vName)
  toJs (FunctionCall (Just (Variable vName _)) (Right (SimpleVar "new")) [HashTable kvPairs] block) = print4 "<%%>%</%>" (tagName vName) (pairsToReact kvPairs) (_maybe toReact block) (tagName vName)
    where pairsToReact pairs = case pairs of
            [] -> ""
            _ -> " " ++ (join " " . map toPair $ pairs)
          toPair ((SaltyString s), (SaltyString v)) = print2 "%=\"%\"" s v
          toPair ((SaltyString s), v) = print2 "%={%}" s (toJs v)
          toPair (k, (SaltyString v)) = print2 "%=\"%\"" (toJs k) v
          toPair (k, v) = print2 "%={%}" (toJs k) (toJs v)

  -- functions called on an obj
  -- toJs (FunctionCall (Just (Variable (ClassVar obj) _)) (Right funcName) args Nothing) = print3 "%.%(%)" obj (simpleVarName funcName) (intercalate ", " . map toJs $ args)
  toJs (FunctionCall (Just var@(Variable _ _)) (Right funcName) args Nothing) = print3 "%.%(%)" (toJs var) (simpleVarName funcName) (intercalate ", " . map toJs $ args)
  toJs (FunctionCall varOrNothing (Right funcName) args (Just braces@(Braces arr))) = toJs (FunctionCall varOrNothing (Right funcName) args (Just (LambdaFunction [] braces)))

  toJs (FunctionCall (Just var@(Variable _ _)) (Right funcName) args (Just block@(LambdaFunction _ _))) = print3 "%.%(%)" (toJs var) (simpleVarName funcName) args_
    where args_ = join ", " $ (map toJs args) ++ [(addReturn block)]

  toJs (FunctionCall (Just obj) (Right funcName) args _) = print3 "%.%(%)" (toJs obj) (simpleVarName funcName) (intercalate ", " . map toJs $ args)

  -- same as above but with parens
  toJs (FunctionCall (Just (Parens [obj])) (Right funcName) args _) = print3 "(%).%(%)" (toJs obj) (simpleVarName funcName) (intercalate ", " . map toJs $ args)

  toJs (LambdaFunction ["_"] body) = print2 "(%) => {\n%\n}" "" (addReturn body)
  toJs (LambdaFunction args body) = print2 "(%) => {\n%\n}" args_ (addReturn body)
    where args_ = join ", " args

  -- static var == react state
  toJs (HigherOrderFunctionCall var@(Variable (StaticVar name) scope) Each lambda@(LambdaFunction loopVar body) accVar)  = line1 ++ line2 ++ "\n" ++ line3
      where line1 = print2 "let % = this.state.%.slice()\n" name name
            line2 = toJs $ HigherOrderFunctionCall (Variable (SimpleVar name) scope) Each lambda accVar
            line3 = print2 "this.setState({\n  %: %\n})" name name

  -- special case, each with a range
  toJs (HigherOrderFunctionCall (Range left right) Each (LambdaFunction loopVar body) _)  =
                print6 "for (let % = %; % <= %; %++) {\n%\n}" (formatLoopVars loopVar) (toJs left) (formatLoopVars loopVar) (toJs right) (formatLoopVars loopVar) (toJs body)
  toJs (HigherOrderFunctionCall (Parens [Range left right]) Each (LambdaFunction loopVar body) _)  =
                print6 "for (let % = %; % <= %; %++) {\n%\n}" (formatLoopVars loopVar) (toJs left) (formatLoopVars loopVar) (toJs right) (formatLoopVars loopVar) (toJs body)

  -- each
  toJs (HigherOrderFunctionCall obj Each (LambdaFunction loopVar body) _)  =
                print3 "%.forEach((%) => {\n%\n})" (toJs obj) (formatLoopVars loopVar) (toJs body)

  -- map
  toJs (HigherOrderFunctionCall obj Map (LambdaFunction loopVar (Braces body)) accVar) =
                print5 "%%.map((%) => {\n%\nreturn %\n})" (getAccVar accVar) (toJs obj) (formatLoopVars loopVar) (initToJs body) (stripNewlineToJs (last body))

  toJs (HigherOrderFunctionCall obj Map (LambdaFunction loopVar body) accVar) =
                print4 "%%.map((%) => %)" (getAccVar accVar) (toJs obj) (formatLoopVars loopVar) (toJs body)

  -- select
  toJs (HigherOrderFunctionCall obj Select (LambdaFunction loopVar (Braces body)) accVar) =
                print5 "%%.filter((%) => {\n%\nreturn %\n})" (getAccVar accVar) (toJs obj) (formatLoopVars loopVar) (initToJs body) (stripNewlineToJs (last body))

  toJs (HigherOrderFunctionCall obj Select (LambdaFunction loopVar body) accVar) =
                print4 "%%.filter((%) => %)" (getAccVar accVar) (toJs obj) (formatLoopVars loopVar) (toJs body)
  -- any
  toJs (HigherOrderFunctionCall obj Any (LambdaFunction loopVar (Braces body)) accVar) =
                print5 "%%.some((%) => {\n%\nreturn %\n})" (getAccVar accVar) (toJs obj) (formatLoopVars loopVar) (initToJs body) (stripNewlineToJs (last body))

  toJs (HigherOrderFunctionCall obj Any (LambdaFunction loopVar body) accVar) =
                print4 "%%.some((%) => %)" (getAccVar accVar) (toJs obj) (formatLoopVars loopVar) (toJs body)

  -- all
  toJs (HigherOrderFunctionCall obj All (LambdaFunction loopVar (Braces body)) accVar) =
                print5 "%%.every((%) => {\n%\nreturn %\n})" (getAccVar accVar) (toJs obj) (formatLoopVars loopVar) (initToJs body) (stripNewlineToJs (last body))

  toJs (HigherOrderFunctionCall obj All (LambdaFunction loopVar body) accVar) =
                print4 "%%.every((%) => %)" (getAccVar accVar) (toJs obj) (formatLoopVars loopVar) (toJs body)

  toJs Salt = "I'm salty"
  toJs (ReturnStatement s) = "return " ++ (toJs s)
  toJs (ReturnStatementForAddReturn s) = addReturn s
  toJs (Parens s) = "(" ++ (concat $ map toJs s) ++ ")"
  toJs (Braces s) = join "\n" $ map toJs s
  toJs (PurePhp line) = line
  toJs (PhpComment str) = "// " ++ str ++ "\n"
  toJs (SaltyComment str) = ""
  toJs (Negate s) = "!" ++ (toJs s)
  toJs EmptyLine = ""
  toJs (BackTrack s) = toJs s

  toJs (If cond thenFork (Just elseFork)) = print3 "if (%) {\n%\n} else {\n%\n}" (concat . map toJs $ cond) (toJs thenFork) (toJs elseFork)
  toJs (If cond thenFork Nothing) = print2 "if (%) {\n%\n}" (concat . map toJs $ cond) (toJs thenFork)
  toJs (While cond body) = print2 "while (%) {\n%\n}" (toJs cond) (toJs body)
  toJs (Class name Nothing Nothing body) = print2 "class % {\n%\n}" (toJs name) (toJs body)
  toJs (Class name (Just extendsName) Nothing body) = print3 "class % extends % {\n%\n}" (toJs name) (toJs extendsName) (toJs body)
  toJs (Class name Nothing (Just implementsName) body) = print3 "class % implements % {\n%\n}" (toJs name) (toJs implementsName) (toJs body)
  toJs (Class name (Just extendsName) (Just implementsName) body) = print4 "class % extends % implements % {\n%\n}" (toJs name) (toJs extendsName) (toJs implementsName) (toJs body)
  toJs (New name args) = print2 "new %(%)" (toJs name) (intercalate "," . map toJs $ args)

  toJs (Variable x _) = toJs x
  toJs (WithNewLine s) = (toJs s) ++ "\n"
  toJs (HashLookup h k) = print2 "%[%]" (toJs h) (toJs k)
  toJs (FunctionTypeSignature var types) = ""

  toJs (Constant var@(Variable name _)) = simpleVarName name
  toJs (Constant _) = "not sure what this constant is"

  toJs (HashTable nameValuePairs) = "{\n" ++ hashBody ++ "\n}"
    where kvtoJs (name, val) = print2 "%: %" (toJs name) (toJs val)
          hashBody = intercalate ",\n" $ map kvtoJs nameValuePairs

  toJs (DestructuredHash vars) = "{ " ++ (join ", " vars) ++ " }"

  toJs (ArraySlice obj start Nothing) = print2 "%.slice(%)" (toJs obj) (toJs start)
  toJs (ArraySlice obj (SaltyNumber start) (Just (SaltyNumber end))) = print3 "%.slice(%, %)" (toJs obj) start newEnd
    where newEnd = show $ (read end :: Integer) + 1
  toJs (ArraySlice obj start (Just end)) = print3 "%.slice(%, %-1)" (toJs obj) (toJs start) (toJs end)
  toJs (StringSlice obj start Nothing) = print2 "%.substring(%)" (toJs obj) (toJs start)
  toJs (StringSlice obj (SaltyNumber start) (Just (SaltyNumber end))) = print3 "%.substring(%, %)" (toJs obj) start newEnd
    where newEnd = show $ (read end :: Integer) + 1
  toJs (StringSlice obj start (Just end)) = print3 "%.substring(%, %-1)" (toJs obj) (toJs start) (toJs end)
  toJs (StringIndex obj index) = print2 "%.charAt(%)" (toJs obj) (toJs index)

  toJs (AttrAccess obj attrName) = print2 "%.%" (toJs obj) attrName
  toJs (MultiAssign vars (WithNewLine value)) = toJs $ WithNewLine (MultiAssign vars value)
  toJs (MultiAssign vars value@(Variable _ _)) = (intercalate "\n" . map (\(i,var) -> print3 "% = %[%]" (toJs var) (toJs value) (show i)) $ zip [0..] vars)
  toJs (MultiAssign vars value@(FunctionCall _ _ _ _)) = initResult ++ "\n" ++ multiAssign
      where initResult = "result = " ++ (toJs value)
            multiAssign = (intercalate "\n" . map (\(i,var) -> print2 "% = result[%]" (toJs var) (show i)) $ zip [0..] vars)
  toJs (MultiAssign vars value) = (intercalate "\n" . map (\var -> print2 "% = %" (toJs var) (toJs value)) $ vars)
  -- toJs (AttrAccess (Variable (InstanceVar obj) _) attrName) = print2 "$this->%->%" obj attrName
  -- toJs (AttrAccess (Variable (StaticVar obj) _) attrName) = print2 "static::$%->%" obj attrName
  -- array of arrays
  toJs (Array salties@((Array _):rest)) = "[\n" ++ (intercalate ",\n" . map toJs $ salties) ++ ",\n]"
  toJs (Array salties) = "[" ++ (intercalate ", " . map toJs $ salties) ++ "]"
  toJs (WhereClause salty) = "const " ++ (toJs salty)
  toJs (Guard cond outcome) = print2 "if (%) {\n%\n}" (concat . map toJs $ cond) (addReturnToArray outcome)
  toJs (SaltyGuard Nothing ((Guard cond outcome):[])) = print2 "if (%) {\n%\n}" (concat . map toJs $ cond) (addReturnToArray outcome)
  toJs (SaltyGuard Nothing guards) = initGuards ++ lastGuard
    where initGuards = intercalate " else" . map toJs . init $ guards
          lastGuard = case (last guards) of
                           (Guard [(SaltyString "otherwise")] outcome) -> " else {\n" ++ (addReturnToArray outcome) ++  "\n}"
                           _ -> " else" ++ (toJs (last guards))

  toJs (SaltyGuard (Just val) guards) = print2 "switch (%) {\n%\n}" (toJs val) guardsToJs
    where guardsToJs = concat . map jsFunc $ guards
          jsFunc (Guard cond outcome) = print2 "%:\n  %\n" (toJs_ cond) (join "\n" . map toJs $ outcome)
          toJs_ [(SaltyString "otherwise")] = "default"
          toJs_ cond_ = (join "\n" . map (\c -> "case " ++ (toJs c)) $ cond_)

  toJs (SaltyBool TRUE) = "true"
  toJs (SaltyBool FALSE) = "false"
  toJs SaltyNull = "null"
  toJs (SaltyMagicConstant c) = toJs c
  toJs (Keyword (KwPreceding "const" (Operation (Constant (Variable var _)) op right))) = "const " ++ (toJs (Operation (PurePhp (simpleVarName var)) op right))
  toJs (Keyword (KwPreceding "const" (Constant (Variable var _)))) = "const " ++ (simpleVarName var)
  toJs (Keyword (KwPreceding "const" (Variable var _))) = "const " ++ (simpleVarName var)

  toJs (Keyword (KwPreceding "export" (Keyword (KwPreceding "default" f@(Function n a b v s))))) = "export default function " ++ (toJs (Function n a b v ClassScope))
  toJs (Keyword (KwPreceding str salty)) = str ++ " " ++ (toJs salty)
  toJs (Keyword (KwSimple str)) = str
  toJs (Range (SaltyNumber l) (SaltyNumber r)) = show $ [left..right]
      where left = read l :: Integer
            right = read r :: Integer
  toJs (Range l r) = "a range: (" ++ (toJs l) ++ ".." ++ (toJs r) ++ ")"
  toJs (Keyword x) = "keyword not implemented yet: " ++ (show x)
  toJs (ParseError x) = x ++ "\n"
  toJs SaltySpace = " "
  toJs x = "not implemented yet: " ++ (show x)


addReturn :: Salty -> String
addReturn x@(ReturnStatement _) = toJs x
addReturn x@(Operation (Variable (StaticVar _) _) Equals right) = toJs x
addReturn x@(Operation var@(Variable _ _) Equals h@(HigherOrderFunctionCall obj callName func accVar)) = addReturn (HigherOrderFunctionCall obj callName func (varName var))
addReturn x@(Operation var@(Variable _ _) Equals (WithNewLine(h@(HigherOrderFunctionCall obj callName func accVar)))) = addReturn (HigherOrderFunctionCall obj callName func (varName var))
addReturn x@(Operation _ Equals _) = toJs x
addReturn x@(Operation _ PlusEquals _) = toJs x
addReturn x@(Operation _ MinusEquals _) = toJs x
addReturn x@(Operation _ MultiplyEquals _) = toJs x
addReturn x@(Operation _ DivideEquals _) = toJs x
addReturn x@(Operation arr ArrayPush elem) = toJs x ++ "\nreturn " ++ (toJs arr)
addReturn x@(Operation left OrEquals _) = (toJs x) ++ "\nreturn " ++ (toJs left) ++ ";"
addReturn x@(Operation _ _ _) = "return " ++ (toJs x)
addReturn (If cond thenFork (Just elseFork)) = print3 "if (%) {\n%\n} else {\n%\n}" (concat . map toJs $ cond) (addReturn thenFork) (addReturn elseFork)
addReturn (If cond thenFork Nothing) = print2 "if (%) {\n%\n}" (concat . map toJs $ cond) (addReturn thenFork)
addReturn x@(Braces []) = toJs x
addReturn (Braces s) = (concat . map toJs . init $ s) ++ "\n" ++ (addReturn . last $ s)
addReturn (Variable name scope) = "return " ++ (toJs name)
addReturn (WithNewLine x) = (addReturn x) ++ "\n"
addReturn (Parens [x@(If _ _ _)]) = addReturn x
addReturn p@(Parens x) = "return " ++ (toJs p)
-- this means html will be wrapped in parens automatically
addReturn f@(FunctionCall (Just (Variable vName _)) (Right (SimpleVar "new")) _ _) = "return (" ++ toJs f ++ ")"
addReturn f@(FunctionCall o n a b) = "return " ++ (toJs f)
addReturn h@(HashTable kv) = "return " ++ (toJs h)
addReturn a@(Array xs) = "return " ++ (toJs a)
addReturn f@(HigherOrderFunctionCall _ Each _ _) = toJs f
addReturn f@(HigherOrderFunctionCall _ _ _ accVar)
                | accVar == "$result" = "return " ++ (toJs f) -- result not assigned to anything, return
                | otherwise = (toJs f) ++ "\nreturn " ++ accVar
addReturn a@(AttrAccess _ _) = "return " ++ (toJs a)
addReturn x@(SaltyNumber _) = "return " ++ (toJs x)
addReturn x@(SaltyString _) = "return " ++ (toJs x)
addReturn x@(SaltyBool _) = "return " ++ (toJs x)
addReturn x@(PurePhp _) = "return " ++ (toJs x)
addReturn x@(Negate _) = "return " ++ (toJs x)
addReturn x@(SaltyGuard (Just val) guards) = toJs (SaltyGuard (Just val) newGuards)
  where newGuards = map addReturn_ guards
        addReturn_ (Guard cond outcome) = Guard cond ((init outcome) ++ [ReturnStatementForAddReturn (last outcome)])
addReturn x = toJs x

