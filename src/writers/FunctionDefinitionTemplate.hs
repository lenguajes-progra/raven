module FunctionDefinitionTemplate where

import Data.List
import ExpressionTemplate
import Grammar
import LiteralTypeTemplate
import StatementTransformer
import LoopTemplate

funcDefTypeTransformer :: Type -> Identifier -> Parameters -> String
funcDefTypeTransformer typ identifier parameters = identifierTransformer identifier ++ " :: " ++ 
  (if parametersTypeTransformer parameters == [] then "" else (intercalate " -> " (parametersTypeTransformer parameters) ++ " -> ")) ++ 
  typeTransformer typ ++ "\n"

funcDefBodyTransformer :: Identifier -> Parameters -> Block -> Expression -> String
funcDefBodyTransformer identifier parameters block expression = identifierTransformer identifier ++ " " ++ intercalate " " (parametersIdentifierTransformer parameters) ++ " = " ++ if blockTransformer block /= "" then expressionTransformer expression ++ "\n\twhere\n\t\t" ++ blockTransformer block else expressionTransformer expression

functionDefinitionTransformer :: FunctionDefinition -> String
functionDefinitionTransformer (FuncDefinition typ identifier parameters block expression) = funcDefTypeTransformer typ identifier parameters ++ funcDefBodyTransformer identifier parameters block expression
functionDefinitionTransformer _ = ""

parametersIdentifierTransformer :: Parameters -> [String]
parametersIdentifierTransformer (Parameters []) = []
parametersIdentifierTransformer (Parameters ((_, identifierParameter) : ps)) = identifierTransformer identifierParameter : parametersIdentifierTransformer (Parameters ps)

parametersTypeTransformer :: Parameters -> [String]
parametersTypeTransformer (Parameters []) = []
parametersTypeTransformer (Parameters ((typeParameter, _) : ps)) = typeTransformer typeParameter : parametersTypeTransformer (Parameters ps)

blockTransformer :: Block -> String
blockTransformer (Block []) = ""
blockTransformer (Block ((VariableDefinition (VariableDefinitionWithoutAssignment _ ident)) : (IfStat is) : _)) = identifierTransformer ident ++ " = " ++ ifTransformer is ++ "\n"
blockTransformer (Block ((VariableDefinition (VariableDefinitionWithoutAssignment _ ident)) : (ForStat fs) : _)) = identifierTransformer ident ++ " = " ++ loopTransformer fs ++ "\n"
blockTransformer (Block (statement : statements)) = "\t\t" ++ statementTransformer statement ++ blockTransformer (Block statements)

