module AbstractSyntaxTree where
  data Variable =
      SimpleVariable String
    | UnitVariable
    deriving (Eq, Show)

  data Expression =
      VariableExpression Variable
    | UnitExpression
    | Let [Declaration] [Expression]
    deriving (Eq, Show)

  data Declaration =
      FunctionDeclaration
    | VariableDeclaration
    | TypeDeclaration
    deriving (Eq, Show)
