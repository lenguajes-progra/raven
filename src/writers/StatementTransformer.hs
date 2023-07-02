module StatementTransformer where

import ArrayDefinitionTemplate
import DataTransformer
import ExpressionTemplate
import Grammar
import PrintStatementTemplate
import VariableDefTemplate
import LoopTemplate

dataVariableTransformer :: VariableType -> String
dataVariableTransformer (TriNode tp ident expr) = expr
dataVariableTransformer (TwiceNodeWithoutAssignment tp ident) = ""
dataVariableTransformer (TwiceNodeWithAssignment ident expr) = expr

dataArrayTransformer :: VariableType -> String
dataArrayTransformer (TriNode tp ident expr) = expr
dataArrayTransformer (TwiceNodeWithoutAssignment tp ident) = ""
dataArrayTransformer (TwiceNodeWithAssignment ident expr) = expr

statementTransformer :: Statement -> String
statementTransformer (VariableDefinition vd) = dataVariableTransformer (variableDefinitionTransformer vd)
statementTransformer (ArrayDefinition ad) = dataArrayTransformer (arrayDefinitionTransformer ad)
statementTransformer (PrintStat ps) = printStatementTransformer ps
statementTransformer (FuncCallStat fcs) = functionCallTransformer fcs
statementTransformer (IfStat ifs) = ifTransformer ifs
statementTransformer (ForStat for) = loopTransformer for
statementTransformer _ = ""

ifTemplate :: String -> String -> String -> String
ifTemplate expression stat1 stat2 = "if " ++ expression ++ " then " ++ stat1 ++ " else " ++ stat2

ifTransformer :: IfStatement -> String
ifTransformer (IfStatement expr stat1 stat2) = "if " ++ expressionTransformer expr ++ " then " ++ statementTransformer stat1 ++ " else " ++ statementTransformer stat2
