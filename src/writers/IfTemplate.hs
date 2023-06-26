module IfTemplate where
-- import Grammar

type Expression = String
type Block = String

ifTemplate :: Expression -> Block -> Block -> String
ifTemplate ex bl1 bl2 = "if" ++ " " ++ ex ++ " " ++ "then" ++ " " ++ bl1 ++ " " ++ "else" ++ " " ++ bl2