{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module AbstractSyntaxTree where
  import Text.PrettyPrint.GenericPretty

  type Symbol = String

  data Variable =
      SimpleVariable Symbol
    | FieldVariable {fieldRecord :: Variable
                    ,fieldMember :: Symbol
                    }
    | SubscriptVariable {subscriptArray :: Variable
                        ,subscriptIndex :: Expression
                        }
    deriving (Eq, Show, Generic, Out)

  data Expression =
      VariableExpression Variable
    | NilExpression
    | IntegerExpression Integer
    | StringExpression String
    | CallExpression {callFunction :: Symbol
                     ,callArguments :: [Expression]
                     }
    | ArithmeticExpression ArithmeticOperator Expression Expression
    | ComparisonExpression ComparisonOperator Expression Expression
    | RecordExpression {recordType :: Symbol
                       ,recordFields :: [(Symbol, Expression)]
                       }
    | SequenceExpression [Expression]
    | AssignExpression {assignTo :: Variable
                       ,assignValue :: Expression
                       }
    | IfExpression {ifTest :: Expression
                   ,ifThen :: Expression
                   ,ifElse :: Maybe Expression
                   }
    | WhileExpression {whileTest :: Expression
                      ,whileBody :: Expression
                      }
    | ForExpression {forVariable :: Symbol
                    ,forLow :: Expression
                    ,forHigh :: Expression
                    ,forBody :: Expression
                    }
    | BreakExpression
    | LetExpression [DeclarationGroup] [Expression]
    | ArrayExpression {arrayType :: Symbol
                      ,arraySize :: Expression
                      ,arrayInit :: Expression
                      }
    deriving (Eq, Show, Generic, Out)

  data ArithmeticOperator = Add | Subtract | Multiply | Divide
    deriving (Eq, Show, Generic, Out)

  data ComparisonOperator = Equals | NotEquals
                          | Less | LessOrEquals
                          | Greater | GreaterOrEquals
    deriving (Eq, Show, Generic, Out)

  data DeclarationGroup =
      FunctionDeclarationGroup [FunctionDeclaration]
    | VariableDeclaration {variableName :: Symbol
                          ,variableType :: Maybe Symbol
                          ,variableInit :: Expression
                          }
    | TypeDeclarationGroup [TypeDeclaration]
    deriving (Eq, Show, Generic, Out)

  data FunctionDeclaration =
      FunctionDeclaration {functionName :: Symbol
                          ,functionParams :: [Field]
                          ,functionResult :: Maybe Symbol
                          ,functionBody :: Expression
                          }
    deriving (Eq, Show, Generic, Out)

  data TypeDeclaration =
      TypeDeclaration {typeName :: Symbol
                      ,typeType :: Type
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
