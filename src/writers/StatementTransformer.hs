module StatementTransformer where

import ArrayDefinitionTemplate
import DataTransformer
import ExpressionTemplate
import Grammar
import PrintStatementTemplate
import VariableDefTemplate
import IfTemplate
import LoopTemplate

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
statementTransformer (IfStat is) =
  ifTransformer is
statementTransformer (ForStat for) =
  loopTransformer for
statementTransformer _ = ""

