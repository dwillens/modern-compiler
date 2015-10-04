{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module AbstractSyntaxTree where
  import Text.PrettyPrint.GenericPretty

  type Symbol = String

  data Variable =
      SimpleVariable Symbol
    | UnitVariable
    deriving (Eq, Show, Generic, Out)

  data Expression =
      VariableExpression Variable
    | IntegerExpression Integer
    | Let [Declaration] [Expression]
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
      NamedType
    | RecordType
    | ArrayType Symbol
    deriving (Eq, Show, Generic, Out)
