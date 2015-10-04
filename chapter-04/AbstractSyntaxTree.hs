{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module AbstractSyntaxTree where
  import Text.PrettyPrint.GenericPretty

  type Symbol = String

  data Variable =
      SimpleVariable Symbol
    | FieldVariable Variable Symbol
    | SubscriptVariable Variable Expression
    deriving (Eq, Show, Generic, Out)

  data Expression =
      VariableExpression Variable
    | IntegerExpression Integer
    | StringExpression String
    | RecordExpression {recordType :: Symbol
                       ,recordFields :: [(Symbol, Expression)]
                       }
    | AssignExpression {assignVariable :: Variable
                       ,assignExpression :: Expression
                       }
    | LetExpression [Declaration] [Expression]
    | ArrayExpression {arrayType :: Symbol
                      ,arraySize :: Expression
                      ,arrayInit :: Expression
                      }
    | UnitExpression
    deriving (Eq, Show, Generic, Out)

  data Declaration =
      FunctionDeclaration
    | VariableDeclaration {vdName :: Symbol
                          ,vdType :: Maybe Symbol
                          ,vdInit :: Expression
                          }
    | TypeDeclaration {tdName :: Symbol
                      ,tdType :: Type
                      }
    deriving (Eq, Show, Generic, Out)

  data Type =
      NamedType Symbol
    | RecordType [Field]
    | ArrayType Symbol
    deriving (Eq, Show, Generic, Out)

  data Field =
      Field {fieldName :: Symbol
            ,fieldType :: Symbol
            }
    deriving (Eq, Show, Generic, Out)
