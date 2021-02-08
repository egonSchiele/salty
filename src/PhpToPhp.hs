module PhpToPhp where
import Types
import Print
import Data.List (intercalate)
import Utils (isConstant)

-- phpToPhp_ indentAmt xs = concat $ map (\x -> show x ++ "\n") xs
phpToPhp_ indentAmt xs = concat $ map showPhp xs

showPhp (Operation (Constant (Variable (SimpleVar "TEMPLATE") _)) Equals (SaltyString templateName)) = templateName
showPhp _ = ""
