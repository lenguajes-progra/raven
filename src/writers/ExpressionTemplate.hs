module ExpressionTemplate where

import Grammar
import LiteralTypeTemplate

termBitAndBoolTransformer :: Expression -> String
termBitAndBoolTransformer (Literal (IntegerLiteral il)) = show il
termBitAndBoolTransformer (Literal _) = throwBitError
termBitAndBoolTransformer (Identifier iden) = identifierTransformer iden
termBitAndBoolTransformer (BitExpression be) = bitExpressionTransformer be
termBitAndBoolTransformer (Parens (BitExpression be)) = putParens (bitExpressionTransformer be)
termBitAndBoolTransformer (Parens (Literal (IntegerLiteral il))) = putParens (show il)
termBitAndBoolTransformer (Parens (Literal _)) = throwBitError
termBitAndBoolTransformer _ = throwBitError

putParens :: String -> String
putParens xs = "(" ++ xs ++ ")"

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
bitExpressionTransformer (BitNot expr) = putParens (bitOpTransformer NotBit ++ termBitAndBoolTransformer expr)
bitExpressionTransformer (BitOp leftExpr bitOperator rightExpr) =
  termBitAndBoolTransformer leftExpr ++ bitOpTransformer bitOperator ++ termBitAndBoolTransformer rightExpr

throwBitError :: a
throwBitError = error "Only operation with number allowed"

-- ! Logic Expression

logicOpTransformer :: LogicalOperator -> String
logicOpTransformer s = case s of
  And -> "&&"
  Or -> "||"
  Not -> "not "

logicExpressionTransformer :: LogicalExpression -> String
logicExpressionTransformer (LogicNot expr) = putParens (logicOpTransformer Not ++ termLogicTransformer expr)
logicExpressionTransformer (LogicOp leftExpr logicOp rightExpr) =
  termLogicTransformer leftExpr ++ logicOpTransformer logicOp ++ termLogicTransformer rightExpr

throwLogicError :: a
throwLogicError = error "Only logical operations are allowed"

termLogicTransformer :: Expression -> String
termLogicTransformer (Literal (BooleanLiteral bl)) = show bl
termLogicTransformer (Literal _) = throwLogicError
termLogicTransformer (Identifier iden) = identifierTransformer iden
termLogicTransformer (BooleanExpression be) = boolExpressionTransformer be
termLogicTransformer (LogicalExpression le) = logicExpressionTransformer le
termLogicTransformer (Parens (BooleanExpression be)) = putParens (boolExpressionTransformer be)
termLogicTransformer (Parens (LogicalExpression le)) = putParens (logicExpressionTransformer le)
termLogicTransformer (Parens (Literal (BooleanLiteral bl))) = putParens (show bl)
termLogicTransformer (Parens (Literal _)) = throwLogicError
termLogicTransformer _ = throwLogicError

-- ! Expression

expressionTransformer :: Expression -> String
expressionTransformer (BooleanExpression be) = boolExpressionTransformer be
expressionTransformer (LogicalExpression le) = logicExpressionTransformer le
expressionTransformer (BitExpression be) = bitExpressionTransformer be
expressionTransformer expr = termTransformer expr

termTransformer :: Expression -> String
termTransformer (Literal lit) = literalTransformer lit
termTransformer (Identifier iden) = identifierTransformer iden
termTransformer (Parens expr) = putParens (expressionTransformer expr)
termTransformer _ = throwBitError

-- ! Function Call

parameterOption :: ParameterOption -> String
parameterOption (ParamLiteral literal) = literalTemplate literal
parameterOption (ParamIdentifier ident) = identifierTransformer ident

functionCallTransformer :: FunctionCall -> String
functionCallTransformer (FunctionCall ident (ParametersCalled options)) =
  putParens (identifierTransformer ident ++ concatMap (\x -> " " ++ parameterOption x) options)