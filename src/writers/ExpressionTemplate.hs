module ExpressionTemplate where

import Expression
import Grammar
import LiteralTemplate (literalTemplate)
import Parsers
import VariableDefTemplate (identifierTransformer)

boolOpTransformer :: BooleanOperator -> String
boolOpTransformer s = case s of
  Equal -> "=="
  NotEqual -> "!="
  LessThan -> "<"
  GreaterThan -> ">"
  LessEqualThan -> "<="
  GreatEqualThan -> ">="

boolExpressionTransformer :: BooleanExpression -> String
boolExpressionTransformer (BooleanOp leftExpr operation rightExpr) =
  termTransformer leftExpr ++ boolOpTransformer operation ++ expressionTransformer rightExpr

bitOpTransformer :: BitOperator -> String
bitOpTransformer s = case s of
  AndBit -> "&"
  OrBit -> "|"
  XorBit -> "^"
  LeftShift -> "<<"
  RightShift -> ">>"
  NotBit -> "~"

bitExpressionTransformer :: BitExpression -> String
bitExpressionTransformer (BitNot expr) = bitOpTransformer NotBit ++ termTransformer expr
bitExpressionTransformer (BitOp leftExpr bitOperator rightExpr) =
  termTransformer leftExpr ++ bitOpTransformer bitOperator ++ expressionTransformer rightExpr

logicOpTransformer :: LogicalOperator -> String
logicOpTransformer s = case s of
  And -> "&&"
  Or -> "||"
  Not -> "!"

logicExpressionTransformer :: LogicalExpression -> String
logicExpressionTransformer (LogicNot expr) = logicOpTransformer Not ++ termTransformer expr
logicExpressionTransformer (LogicOp leftExpr logicOp rightExpr) =
  termTransformer leftExpr ++ logicOpTransformer logicOp ++ expressionTransformer rightExpr

expressionTransformer :: Expression -> String
expressionTransformer (BooleanExpression be) = boolExpressionTransformer be
expressionTransformer (LogicalExpression le) = logicExpressionTransformer le
expressionTransformer (BitExpression be) = bitExpressionTransformer be
expressionTransformer expr = termTransformer expr

termTransformer :: Expression -> String
termTransformer (Literal lit) = literalTemplate lit
termTransformer (Identifier iden) = identifierTransformer iden
termTransformer (Parens expr) = "(" ++ expressionTransformer expr ++ ")"