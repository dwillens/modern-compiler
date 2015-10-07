module TypeChecker(typeCheck) where
  import Control.Monad
  import Data.Either
  import Data.Foldable
  import qualified Data.Map.Strict as M
  import Data.Maybe
  import Data.Unique

  import qualified AbstractSyntaxTree as AST

  data Type = Int
            | String
            | Record [(String, Type)] Unique
            | Array Type Unique
            | Nil
            | Unit
            | Placeholder String (Maybe Type)
    deriving (Eq)

  data Value = Variable Type | Function [Type] Type
    deriving (Eq)

  type Values = M.Map String Value
  type Types = M.Map String Type
  type Environment = (Values, Types)

  insertValue :: Environment -> AST.Identifier -> Value -> Environment
  insertValue (vs, ts) (name, _) ty =  (M.insert name ty vs, ts)

  typeCheck :: AST.Expression -> Either String ()
  typeCheck = void . expression (M.empty, M.empty)

  expression :: Environment -> AST.Expression -> Either String Type
  expression env (AST.VariableExpression v) = variable env v

  expression _ (AST.IntegerExpression _ _) = return Int
  expression _ (AST.StringExpression _ _) = return String


  expression env (AST.ArithmeticExpression _ l r p) =
    expression env l >>= operandType env
      >> expression env r >>= operandType env
    where operandType _ Int = return Int
          operandType _ _ = Left $ show p ++ ": operand must be int"

  expression env (AST.SequenceExpression es p) = expressions env es

  expression env (AST.AssignExpression to val p) =
    do toType <- variable env to
       valType <- expression env val
       if toType == valType
          then return Unit
          else Left (show p ++ ": assignment types must match")

  expression env (AST.ForExpression v low hi body p) =
    expression env low >>= boundType env
      >> expression env hi >>= boundType env
      >> expression env' body >>= bodyType env
    where env' = insertValue env v (Variable Int)
          boundType _ Int = return Int
          boundType _ _ = Left $ show p ++ ": for loop bound must be int"
          bodyType _ Unit = return Unit
          bodyType _ _ = Left $ show p ++ ": for loop body must be unit"

  expression env (AST.LetExpression ds es p) =
    foldM declaration env ds >>= flip expressions es

  expression _ node = Left ("Can't check expression:\n" ++ show node)

  expressions :: Environment -> [AST.Expression] -> Either String Type
  expressions env = foldl (lastType $ expression env) (return Unit)
    where lastType :: Monad m => (a -> m b) -> m b -> a -> m b
          lastType f p x = p *> f x

  declaration :: Environment -> AST.Declaration -> Either String Environment
  declaration env (AST.VariableDeclaration name Nothing init p) =
    expression env init >>= return . insertValue env name . Variable
  declaration _ node = Left ("Can't check declaration:\n" ++ show node)

  variable :: Environment -> AST.Variable -> Either String Type
  variable (vs, _) (AST.SimpleVariable (name, p)) =
    case M.lookup name vs of
      Just (Variable t) -> return t
      Just _ -> Left $ show p ++ ": expected a variable"
      Nothing -> Left $ show p ++ ": could not find variable"
