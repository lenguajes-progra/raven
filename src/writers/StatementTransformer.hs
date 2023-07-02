module StatmentTransformer where

import ArrayDefinitionTemplate
import DataTransformer
import ExpressionTemplate
import Grammar
import PrintStatementTemplate
import VariableDefTemplate

dataTransformer :: VariableType -> (String -> String -> String -> String) -> String
dataTransformer (TriNode tp ident expr) f = f tp ident expr
dataTransformer (TwiceNode _ _) _ = ""

statementTransformer :: Statement -> String
statementTransformer (VariableDefinition vd) =
  dataTransformer (variableDefinitionTransformer vd) variableDefinitionTemplate
statementTransformer (ArrayDefinition ad) =
  dataTransformer (arrayDefinitionTransformer ad) arrayTemplate
statementTransformer (PrintStat ps) =
  printStatementTransformer ps
statementTransformer (FuncCallStat fcs) =
  functionCallTransformer fcs
statementTransformer (Expression expr) =
  expressionTransformer expr
statementTransformer _ = ""