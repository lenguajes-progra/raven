module StatementTransformer where

import ArrayDefinitionTemplate
import DataTransformer
import ExpressionTemplate
import Grammar
import LoopTemplate
import PrintStatementTemplate
import VariableDefTemplate

dataVariableIfTransformer :: VariableType -> String
dataVariableIfTransformer (TriNode {}) = "error \"You cannot declare variables here\""
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
  ('[' : _) -> "[]"
  _ -> ""

statementTransformer :: Statement -> [FunctionDefinition] -> String
statementTransformer (VariableDefinition vd) fdl = dataVariableTransformer (variableDefinitionTransformer vd fdl)
statementTransformer (ArrayDefinition ad) _ = dataVariableTransformer (arrayDefinitionTransformer ad)
statementTransformer (PrintStat ps) fdl = printStatementTransformer ps fdl
statementTransformer (FuncCallStat fcs) fdl = functionCallTransformer fcs fdl
statementTransformer (IfStat ifs) fdl = ifTransformer ifs fdl
statementTransformer (ForStat for) fdl = loopTransformer for fdl
statementTransformer _ _ = ""

statementIfTransformer :: Statement -> [FunctionDefinition] -> String
statementIfTransformer (VariableDefinition vd) fd = dataVariableIfTransformer (variableDefinitionTransformer vd fd)
statementIfTransformer (ArrayDefinition ad) _ = dataVariableIfTransformer (arrayDefinitionTransformer ad)
statementIfTransformer _ _ = ""

ifTemplate :: String -> String -> String -> String
ifTemplate expression stat1 stat2 = "if " ++ expression ++ " then " ++ stat1 ++ " else " ++ stat2

ifTransformer :: IfStatement -> [FunctionDefinition] -> String
ifTransformer (IfStatement expr stat1 stat2) fd = "if " ++ expressionTransformer expr fd ++ " then " ++ statementIfTransformer stat1 fd ++ " else " ++ statementIfTransformer stat2 fd
