module StatementTransformer where

import ArrayDefinitionTemplate
import DataTransformer
import ExpressionTemplate
import Grammar
import PrintStatementTemplate
import VariableDefTemplate
import LoopTemplate

dataVariableIfTransformer :: VariableType -> String
dataVariableIfTransformer (TriNode _ _ _) = "error \"You cannot declare variables here\""
dataVariableIfTransformer (TwiceNodeWithoutAssignment _ _) = "error \"You cannot declare variables here\""
dataVariableIfTransformer (TwiceNodeWithAssignment _ expr) = expr

dataVariableTransformer :: VariableType -> String
dataVariableTransformer (TriNode tp ident expr) = variableDefinitionTemplate tp ident expr ++ "\n"
dataVariableTransformer (TwiceNodeWithoutAssignment tp ident) = ident ++ " = " ++ verifyType tp ++ "\n"
dataVariableTransformer (TwiceNodeWithAssignment ident expr) = ident ++ " = " ++ expr ++ "\n"

-- valores por defecto
verifyType :: String -> String
verifyType x = case x of
  "String" -> "\"\""
  "Int" -> "0"
  "Bool" -> "false"
  "Char" -> "\'\'"
  ('[':_) -> "[]"
  _ -> ""

statementTransformer :: Statement -> String
statementTransformer (VariableDefinition vd) = dataVariableTransformer (variableDefinitionTransformer vd)
statementTransformer (ArrayDefinition ad) = dataVariableTransformer (arrayDefinitionTransformer ad)
statementTransformer (PrintStat ps) = printStatementTransformer ps
statementTransformer (FuncCallStat fcs) = functionCallTransformer fcs
statementTransformer (IfStat ifs) = ifTransformer ifs
statementTransformer (ForStat for) = loopTransformer for
statementTransformer _ = ""

statementIfTransformer :: Statement -> String
statementIfTransformer (VariableDefinition vd) = dataVariableIfTransformer (variableDefinitionTransformer vd)
statementIfTransformer (ArrayDefinition ad) = dataVariableIfTransformer (arrayDefinitionTransformer ad)
statementIfTransformer _ = ""

ifTemplate :: String -> String -> String -> String
ifTemplate expression stat1 stat2 = "if " ++ expression ++ " then " ++ stat1 ++ " else " ++ stat2

ifTransformer :: IfStatement -> String
ifTransformer (IfStatement expr stat1 stat2) = "if " ++ expressionTransformer expr ++ " then " ++ statementIfTransformer stat1 ++ " else " ++ statementTransformer stat2
