module Parser.OperationParser where

import Types
import Utils
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator

operator = debug "operator" >> do
       (string "!=" >> return NotEquals)
  <||> (string "+=" >> return PlusEquals)
  <||> (string "-=" >> return MinusEquals)
  <||> (string "/=" >> return DivideEquals)
  <||> (string "*=" >> return MultiplyEquals)
  <||> (string "[]=" >> return ArrayPush)
  <||> (string "||=" >> return OrEquals)
  <||> (string "||" >> return OrOr)
  <||> (string "&&" >> return AndAnd)
  <||> (string "??" >> return NullCoalesce)
  <||> (string "++" >> return PlusPlus)
  <||> (string "<>" >> return ArrayMerge)
  <||> (string "<->" >> return ArrayDiff)
  <||> (string "instanceof" >> return InstanceOf)
  <||> (string "isa" >> return InstanceOf)
  <||> (string "in" >> return In)
  <||> (string "keyin" >> return KeyIn)
  <||> (string "==" >> return EqualsEquals)
  <||> (string "<=>" >> return Spaceship)
  <||> (string "<=" >> return LessThanOrEqualTo)
  <||> (string ">=" >> return GreaterThanOrEqualTo)
  <||> (string "<" >> return LessThan)
  <||> (string ">" >> return GreaterThan)
  <||> (string "+" >> return Add)
  <||> (string "-" >> return Subtract)
  <||> (string "/" >> return Divide)
  <||> (string "*" >> return Multiply)
  <||> (string "=" >> return Equals)
  <||> (string "%" >> return Modulo)
  <?> "an operator"

operation lhsParser rhsParser = debug "operation" >> do
  indentDebugger
  left <- lhsParser
  unindentDebugger
  space
  op <- operator
  space
  indentDebugger
  right <- rhsParser
  unindentDebugger
  return $ Operation left op right

partialOperation rhsParser = debug "partialOperation" >> do
  leftHandSide <- lastSalty <$> getState
  space
  op <- operator
  space
  indentDebugger
  right <- rhsParser
  unindentDebugger
  return $ BackTrack (Operation leftHandSide op right)

