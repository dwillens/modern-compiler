module AbstractSyntaxTree where
  data Var =
      SimpleVar String
    | FieldVar
    | SubscriptVar
    deriving (Eq, Show)

  data Expression =
      VarExp Var
    | NilExp
    | Unit
    | Let [Declaration] [Expression]
    deriving (Eq, Show)

  data Declaration =
      FunctionDec
    | VarDec
    | TypeDec
    deriving (Eq, Show)
