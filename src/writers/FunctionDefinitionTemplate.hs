module FunctionDefinitionTemplate where

import Data.List
import ExpressionTemplate
import Grammar
import LiteralTypeTemplate
import LoopTemplate
import StatementTransformer

funcDefTypeTransformer :: Type -> Identifier -> Parameters -> String
funcDefTypeTransformer typ identifier parameters =
  identifierTransformer identifier
    ++ " :: "
    ++ (if parametersTypeTransformer parameters == [] then "" else (intercalate " -> " (parametersTypeTransformer parameters) ++ " -> "))
    ++ typeTransformer typ
    ++ "\n"

funcDefBodyTransformer :: Identifier -> Parameters -> Block -> Expression -> [FunctionDefinition] -> String
funcDefBodyTransformer identifier parameters block expression fdl = identifierTransformer identifier ++ " " ++ intercalate " " (parametersIdentifierTransformer parameters) ++ " = " ++ if blockTransformer block fdl /= "" then expressionTransformer expression fdl ++ "\n\twhere\n" ++ blockTransformer block fdl else expressionTransformer expression fdl ++ "\n"

functionDefinitionTransformer :: FunctionDefinition -> [FunctionDefinition] -> String
functionDefinitionTransformer (FuncDefinition typ identifier parameters block expression) fdl = funcDefTypeTransformer typ identifier parameters ++ funcDefBodyTransformer identifier parameters block expression fdl
functionDefinitionTransformer _ _ = ""

parametersIdentifierTransformer :: Parameters -> [String]
parametersIdentifierTransformer (Parameters []) = []
parametersIdentifierTransformer (Parameters ((_, identifierParameter) : ps)) = identifierTransformer identifierParameter : parametersIdentifierTransformer (Parameters ps)

parametersTypeTransformer :: Parameters -> [String]
parametersTypeTransformer (Parameters []) = []
parametersTypeTransformer (Parameters ((typeParameter, _) : ps)) = typeTransformer typeParameter : parametersTypeTransformer (Parameters ps)

blockTransformer :: Block -> [FunctionDefinition] -> String
blockTransformer (Block []) _ = ""
blockTransformer (Block ((VariableDefinition (VariableDefinitionWithoutAssignment _ ident)) : (IfStat is) : _)) fdl =
  identifierTransformer ident ++ " = " ++ ifTransformer is fdl ++ "\n"
blockTransformer (Block ((VariableDefinition (VariableDefinitionWithoutAssignment _ ident)) : (ForStat fs) : _)) fdl =
  identifierTransformer ident ++ " = " ++ loopTransformer fs fdl ++ "\n"
blockTransformer (Block (statement : statements)) fdl =
  statementTransformer statement fdl ++ blockTransformer (Block statements) fdl