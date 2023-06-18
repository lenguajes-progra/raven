module Grammar where

data Literal = IntegerLiteral Integer
             | CharacterLiteral Char
             | StringLiteral String
             | BooleanLiteral Bool
             deriving (Show)

data Identifier = Ident String
  deriving (Show)

data Element = LiteralElement Literal
             | IdentifierElement Identifier
             deriving (Show)

data ElementList = Literals [Literal]
                deriving (Show)

data Type = IntType
          | CharType
          | BooleanType
          | StringType
          | FunctionType
          | ArrayType Type
          deriving (Show)

data VariableDefinition = VariableDefinitionComplete Type Identifier (Either Error Literal)
                         | VariableDefinitionWithoutAssignment Type Identifier
                         | VariableDefinitionWithAssignment Identifier Literal
                         deriving (Show)

data ArrayDefinition = ArrayDefinitionComplete Type Identifier (Either Error ElementList)
                     | ArrayDefinitionWithoutAssignment Type Identifier
                     | ArrayDefinitionWithAssignment Identifier ElementList
                     deriving (Show)

data PrintStatement = PrintStatement Literal
                    deriving (Show)

data IfStatement = IfStatement Expression Block Block deriving (Show)

data Expression
  = Literal Literal
  | Identifier Identifier
  | NumericExpression NumericExpression
  | LogicalExpression LogicalExpression
  | BitExpression BitExpression
  | Parens Expression
  deriving (Show)

data LogicalExpression
  = LogicNot LogicalOperator Expression
  | LogExpr Expression LogicalOperator Expression
  deriving (Show)

data BitExpression
  = BitNot BitOperator Expression
  | BitExpr Expression BitOperator Expression
  deriving (Show)

data NumericExpression
  = NumExpr Expression NumericOperator Expression
  deriving (Show)

data LogicalOperator = And | Or | Not deriving (Show)

data BitOperator = AndBit | OrBit | XorBit | RightShift | LeftShift | NotBit deriving (Show)

data NumericOperator = Equal | NotEqual | LessThan | GreaterThan | LessEqualThan | GreatEqualThan deriving (Show)

data Error = ErrorType ErrorType deriving (Show)

data ErrorType = Syntax
               | Type
               | TypeFunction
               | AssignType
               deriving (Show)

data Comment = CommentLine String
             | CommentBlock String
             deriving (Show)

type Block = String
