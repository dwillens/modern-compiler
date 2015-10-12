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

  reportError :: Show p => p -> String -> Either String a
  reportError p = Left . (++) (show p)

  insertValue :: Environment -> AST.Identifier -> Value -> Environment
  insertValue (vs, ts) (name, _) val = (M.insert name val vs, ts)

  insertType :: Environment -> AST.Identifier -> Type -> Environment
  insertType (vs, ts) (name, _) ty = (vs, M.insert name ty ts)

  typeCheck :: AST.Expression -> Either String ()
  typeCheck = void . expression (M.empty, standardTypes)
    where standardTypes = M.fromList [("int", Int), ("string", String)]

  expression :: Environment -> AST.Expression -> Either String Type
  expression env (AST.VariableExpression v) = variable env v

  expression _ (AST.IntegerExpression _ _) = return Int
  expression _ (AST.StringExpression _ _) = return String

  expression env (AST.ArithmeticExpression _ l r p) =
    expression env l >>= operandType env
      >> expression env r >>= operandType env
    where operandType _ Int = return Int
          operandType _ _ = reportError p ": operand must be int"

  expression env (AST.EqualityExpression _ l r p) =
    do lType <- expression env l
       rType <- expression env r
       typesMatch matchError lType rType
    where matchError = reportError p ": equality types must match"

  expression env (AST.OrderingExpression _ l r p) =
    do lType <- expression env l
       rType <- expression env r
       orderingTypes env lType rType
    where orderingTypes _ Int Int = return Int
          orderingTypes _ String String = return Int
          orderingTypes _ _ _ = reportError p ": cannot order types"

  expression env (AST.SequenceExpression es p) = expressions env es

  expression env (AST.AssignExpression to val p) =
    do toType <- variable env to
       valType <- expression env val
       typesMatch matchError toType valType
    where matchError = reportError p ": assignment types must match"

  expression env (AST.IfExpression test thenE (Just elseE) p) =
    expression env test >>= testType env
      >> do thenType <- expression env thenE
            elseType <- expression env elseE
            typesMatch matchError thenType elseType
    where testType _ Int = return Int
          testType _ _ = reportError p ": if condition must be int"
          matchError = reportError p ": if-then-else types must match"

  expression env (AST.IfExpression test thenE Nothing p) =
    expression env test >>= testType env
      >> expression env thenE >>= thenType env
    where testType _ Int = return Int
          testType _ _ = reportError p ": if condition must be int"
          thenType _ Unit = return Unit
          thenType _ _ = reportError p ": if-then type must be unit"

  expression env (AST.WhileExpression test body p) =
    expression env test >>= testType env
      >> expression env body >>= bodyType env
    where testType _ Int = return Int
          testType _ _ = reportError p ": while condition must be int"
          bodyType _ Unit = return Unit
          bodyType _ _ = reportError p ": while body must be unit"

  expression env (AST.ForExpression v low hi body p) =
    expression env low >>= boundType env
      >> expression env hi >>= boundType env
      >> expression env' body >>= bodyType env
    where env' = insertValue env v (Variable Int)
          boundType _ Int = return Int
          boundType _ _ = reportError p ": for bound must be int"
          bodyType _ Unit = return Unit
          bodyType _ _ = reportError p ": for body must be unit"

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
      Just _ -> reportError p ": expected a variable"
      Nothing -> reportError p ": could not find variable"

  typesMatch :: Either String Type -> Type -> Type -> Either String Type
  typesMatch message a b
    | a == b = return a
    | otherwise = message
