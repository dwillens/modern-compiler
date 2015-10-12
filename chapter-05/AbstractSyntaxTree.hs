{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module AbstractSyntaxTree where
  import Text.PrettyPrint.GenericPretty
  import Position

  type Identifier = (String, Position)

  data Variable =
      SimpleVariable Identifier
    | FieldVariable {fieldRecord :: Variable
                    ,fieldMember :: Identifier
                    ,fieldPosition :: Position
                    }
    | SubscriptVariable {subscriptArray :: Variable
                        ,subscriptIndex :: Expression
                        ,subscriptPosition :: Position
                        }
    deriving (Eq, Show, Generic, Out)

  data Expression =
      VariableExpression Variable
    | NilExpression Position
    | IntegerExpression Integer Position
    | StringExpression String Position
    | CallExpression {callFunction :: Identifier
                     ,callArguments :: [Expression]
                     ,callPosition :: Position
                     }
    | ArithmeticExpression ArithmeticOperator Expression Expression Position
    | EqualityExpression EqualityOperator Expression Expression Position
    | OrderingExpression OrderingOperator Expression Expression Position
    | RecordExpression {recordType :: Identifier
                       ,recordFields :: [(Identifier, Expression)]
                       ,recordPosition :: Position
                       }
    | SequenceExpression [Expression] Position
    | AssignExpression {assignTo :: Variable
                       ,assignValue :: Expression
                       ,assignPosition :: Position
                       }
    | IfExpression {ifTest :: Expression
                   ,ifThen :: Expression
                   ,ifElse :: Maybe Expression
                   ,ifPosition :: Position
                   }
    | WhileExpression {whileTest :: Expression
                      ,whileBody :: Expression
                      ,whilePosition :: Position
                      }
    | ForExpression {forVariable :: Identifier
                    ,forLow :: Expression
                    ,forHigh :: Expression
                    ,forBody :: Expression
                    ,forPosition :: Position
                    }
    | BreakExpression Position
    | LetExpression {letDeclarations :: [Declaration]
                    ,letExpression :: [Expression]
                    ,letPosition :: Position
                    }
    | ArrayExpression {arrayType :: Identifier
                      ,arraySize :: Expression
                      ,arrayInit :: Expression
                      ,arrayPosition :: Position
                      }
    deriving (Eq, Show, Generic, Out)

  data ArithmeticOperator = Add | Subtract | Multiply | Divide
    deriving (Eq, Show, Generic, Out)

  data EqualityOperator = Equals | NotEquals
    deriving (Eq, Show, Generic, Out)

  data OrderingOperator = Less | LessOrEquals
                        | Greater | GreaterOrEquals
    deriving (Eq, Show, Generic, Out)

  data Declaration =
      FunctionDeclarationGroup [FunctionDeclaration]
    | VariableDeclaration {variableName :: Identifier
                          ,variableType :: Maybe Identifier
                          ,variableInit :: Expression
                          ,variablePosition :: Position
                          }
    | TypeDeclarationGroup [TypeDeclaration]
    deriving (Eq, Show, Generic, Out)

  data FunctionDeclaration =
      FunctionDeclaration {functionName :: Identifier
                          ,functionParams :: [Field]
                          ,functionResult :: Maybe Identifier
                          ,functionBody :: Expression
                          ,functionPosition :: Position
                          }
    deriving (Eq, Show, Generic, Out)

  data TypeDeclaration =
      TypeDeclaration {typeName :: Identifier
                      ,typeType :: Type
                      ,typePosition :: Position
                      }
    deriving (Eq, Show, Generic, Out)


  data Type =
      NamedType Identifier
    | RecordType [Field] Position
    | ArrayType Identifier Position
    deriving (Eq, Show, Generic, Out)

  data Field =
      Field {fieldName :: Identifier
            ,fieldType :: Identifier
            }
    deriving (Eq, Show, Generic, Out)
