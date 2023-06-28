module IfTemplate where
import Grammar

-- type Expression = String
-- type Block = String

ifTemplate :: Expression -> Block -> Block -> String
ifTemplate ex bl1 bl2 = "if" ++ " " ++ show ex ++ " " ++ "then" ++ " " ++ show bl1 ++ " " ++ "else" ++ " " ++ show bl2