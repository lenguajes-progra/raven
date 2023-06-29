module Grammar where

data Literal
  = IntegerLiteral Integer
  | CharacterLiteral Char
  | StringLiteral String
  | BooleanLiteral Bool
  deriving (Eq, Show)

data Identifier = Ident String
  deriving (Eq, Show)

data Element
  = LiteralElement Literal
  | IdentifierElement Identifier
  deriving (Eq, Show)

data ElementList = Literals [Literal]
  deriving (Eq, Show)

data Type
  = IntType
  | CharType
  | BooleanType
  | StringType
  | FunctionType
  | ArrayType Type
  deriving (Eq, Show)

data VariableDefinition
  = VariableDefinitionComplete Type Identifier Expression
  | VariableDefinitionWithoutAssignment Type Identifier
  | VariableDefinitionWithAssignment Identifier Expression
  | VariableErrorDefinition Error
  deriving (Eq, Show)

data ArrayDefinition
  = ArrayDefinitionComplete Type Identifier ElementList
  | ArrayDefinitionWithoutAssignment Type Identifier
  | ArrayDefinitionWithAssignment Identifier ElementList
  | ArrayErrorDefinition Error
  deriving (Eq, Show)

data PrintStatement = PrintStatement Expression
  deriving (Eq, Show)

data IfStatement = IfStatement Expression Block Block deriving (Eq, Show)

data Expression
  = Literal Literal
  | Identifier Identifier
  | BooleanExpression BooleanExpression
  | LogicalExpression LogicalExpression
  | BitExpression BitExpression
  | Parens Expression
  | FuncCall FunctionCall
  deriving (Eq, Show)

data LogicalExpression
  = LogicNot Expression
  | LogicOp Expression LogicalOperator Expression
  deriving (Eq, Show)

data BitExpression
  = BitNot Expression
  | BitOp Expression BitOperator Expression
  deriving (Eq, Show)

data BooleanExpression
  = BooleanOp Expression BooleanOperator Expression
  deriving (Eq, Show)

data LogicalOperator = And | Or | Not deriving (Eq, Show)

data BitOperator = AndBit | OrBit | XorBit | RightShift | LeftShift | NotBit deriving (Eq, Show)

data BooleanOperator = Equal | NotEqual | LessThan | GreaterThan | LessEqualThan | GreatEqualThan deriving (Eq, Show)

data Error = ErrorType ErrorType deriving (Eq, Show)

data ErrorType
  = Syntax
  | Type
  | TypeFunction
  | AssignType
  deriving (Eq, Show)

data Comment
  = CommentLine String
  | CommentBlock String
  deriving (Eq, Show)

data Block = Block [Statement]
  deriving (Eq, Show)

data Parameters = Parameters [(Type, Identifier)]
  deriving (Eq, Show)

data FunctionDefinition
  = FuncDefinition Type Identifier Parameters Block Expression
  | FuncDefinitionError Error
  deriving (Eq, Show)

data FunctionDefinitionList = FuncDefList [FunctionDefinition]
  deriving (Eq, Show)

data ParameterOption
  = ParamLiteral Literal
  | ParamIdentifier Identifier
  deriving (Eq, Show)

newtype ParametersCalled = ParametersCalled [ParameterOption] deriving (Eq, Show)

data FunctionCall = FunctionCall Identifier ParametersCalled deriving (Eq, Show)

data Statement
  = Expression Expression
  | VariableDefinition VariableDefinition
  | ArrayDefinition ArrayDefinition
  | IfStat IfStatement
  | LoopStat LoopStatement
  | PrintStat PrintStatement
  | FuncCallStat FunctionCall
  | End Char
  deriving (Eq, Show)

data LoopStatement = LoopStatement Expression Block deriving (Eq, Show)

data Program = Program FunctionDefinitionList Block deriving (Eq, Show)
