module ExpressionTemplate where

import Expression
import Grammar
import LiteralTemplate (literalTemplate)
import Parsers
import VariableDefTemplate (identifierTransformer)

termBitAndBoolTransformer :: Expression -> String
termBitAndBoolTransformer (Literal (IntegerLiteral il)) = show il
termBitAndBoolTransformer (Literal _) = throwBitError
termBitAndBoolTransformer (Identifier iden) = identifierTransformer iden
termBitAndBoolTransformer (BitExpression be) = bitExpressionTransformer be
termBitAndBoolTransformer (Parens (BitExpression be)) = "(" ++ bitExpressionTransformer be ++ ")"
termBitAndBoolTransformer (Parens (Literal (IntegerLiteral il))) = "(" ++ show il ++ ")"
termBitAndBoolTransformer (Parens (Literal _)) = throwBitError
termBitAndBoolTransformer _ = throwBitError

-- ! Boolean Expression
boolOpTransformer :: BooleanOperator -> String
boolOpTransformer s = case s of
  Equal -> "=="
  NotEqual -> "/="
  LessThan -> "<"
  GreaterThan -> ">"
  LessEqualThan -> "<="
  GreatEqualThan -> ">="

boolExpressionTransformer :: BooleanExpression -> String
boolExpressionTransformer (BooleanOp leftExpr operation rightExpr) =
  termBitAndBoolTransformer leftExpr ++ boolOpTransformer operation ++ termBitAndBoolTransformer rightExpr

-- ! Bit Expression

bitOpTransformer :: BitOperator -> String
bitOpTransformer s = case s of
  AndBit -> ".&."
  OrBit -> ".|."
  XorBit -> "^"
  LeftShift -> "`shiftL`"
  RightShift -> "`shiftR`"
  NotBit -> "complement "

bitExpressionTransformer :: BitExpression -> String
bitExpressionTransformer (BitNot expr) = "(" ++ bitOpTransformer NotBit ++ termBitAndBoolTransformer expr ++ ")"
bitExpressionTransformer (BitOp leftExpr bitOperator rightExpr) =
  termBitAndBoolTransformer leftExpr ++ bitOpTransformer bitOperator ++ termBitAndBoolTransformer rightExpr

throwBitError :: a
throwBitError = error "Only operation with number allowed"

-- ! Logic Expression

logicOpTransformer :: LogicalOperator -> String
logicOpTransformer s = case s of
  And -> "&&"
  Or -> "||"
  Not -> "!"

logicExpressionTransformer :: LogicalExpression -> String
logicExpressionTransformer (LogicNot expr) = logicOpTransformer Not ++ termTransformer expr
logicExpressionTransformer (LogicOp leftExpr logicOp rightExpr) =
  termLogicTransformer leftExpr ++ logicOpTransformer logicOp ++ termLogicTransformer rightExpr

throwLogicError :: a
throwLogicError = error "only logical operations are allowed"

termLogicTransformer :: Expression -> String
termLogicTransformer (Literal (BooleanLiteral bl)) = show bl
termLogicTransformer (Literal _) = throwLogicError
termLogicTransformer (Identifier iden) = identifierTransformer iden
termLogicTransformer (BooleanExpression be) = boolExpressionTransformer be
termLogicTransformer (LogicalExpression le) = logicExpressionTransformer le
termLogicTransformer (Parens (BooleanExpression be)) = "(" ++ boolExpressionTransformer be ++ ")"
termLogicTransformer (Parens (LogicalExpression le)) = "(" ++ logicExpressionTransformer le ++ ")"
termLogicTransformer (Parens (Literal (BooleanLiteral bl))) = "(" ++ show bl ++ ")"
termLogicTransformer (Parens (Literal _)) = throwLogicError
termLogicTransformer _ = throwLogicError

-- ! Expression

expressionTransformer :: Expression -> String
expressionTransformer (BooleanExpression be) = boolExpressionTransformer be
expressionTransformer (LogicalExpression le) = logicExpressionTransformer le
expressionTransformer (BitExpression be) = bitExpressionTransformer be
expressionTransformer expr = termTransformer expr

termTransformer :: Expression -> String
termTransformer (Literal lit) = literalTemplate lit
termTransformer (Identifier iden) = identifierTransformer iden
termTransformer (Parens expr) = "(" ++ expressionTransformer expr ++ ")"
termTransformer _ = throwBitError
