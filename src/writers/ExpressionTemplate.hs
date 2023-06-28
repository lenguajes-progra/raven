module ExpressionTemplate where
import Grammar

boolOpTransformer :: BooleanOperator -> String
boolOpTransformer symbol = case symbol of
    Equal -> "=="
    NotEqual -> "!="
    LessThan -> "<"
    GreaterThan -> ">"
    LessEqualThan -> "<="
    GreatEqualThan -> ">="

bitOpTransformer :: BitOperator -> String
bitOpTransformer symbol = case symbol of
    AndBit -> "&"
    OrBit -> "|"
    XorBit -> "^"
    LeftShift -> "<<"
    RightShift -> ">>"
    NotBit -> "~"

logicOpTransformer :: LogicalOperator -> String
logicOpTransformer symbol = case symbol of
    And -> "&&"
    Or -> "||"
    Not -> "!"

