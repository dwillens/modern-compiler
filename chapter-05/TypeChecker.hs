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
            | Record [(String, Type)]
            | Array Type
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

  expression env (AST.SequenceExpression es p) = expressionSequence env es

  expression env (AST.RecordExpression ty fs p) =
    do recType <- resolveType env ty
       forM_ fs $ fieldType env recType
       return recType
    where fieldType :: Environment -> Type -> (AST.Identifier, AST.Expression) -> Either String Type
          fieldType env (Record fs) ((name, _), init) =
            case lookup name fs of
              Just t -> expression env init >>= typesMatch matchError t
              Nothing -> reportError p ": field not found"
          fieldType _ _ _ = reportError p ": expected record type"
          matchError = reportError p ": field init type must match"

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
    foldM declaration env ds >>= flip expressionSequence es

  expression env(AST.ArrayExpression ty size init p) =
    do arrayType <- resolveType env ty
       initType <- expression env init
       arrayTypesMatch arrayType initType
    where arrayTypesMatch t@(Array array) init
            | array == init = return t
            | otherwise = reportError p ": array init type must match"
          arrayTypesMatch _ _ = reportError p ": expected an array expression"

  expression _ node = reportError node "\nCan't check expression"

  expressionSequence :: Environment -> [AST.Expression] -> Either String Type
  expressionSequence env = foldl (lastType $ expression env) (return Unit)
    where lastType :: Monad m => (a -> m b) -> m b -> a -> m b
          lastType f p x = p *> f x


  variable :: Environment -> AST.Variable -> Either String Type
  variable (vs, _) (AST.SimpleVariable (name, p)) =
    case M.lookup name vs of
      Just (Variable t) -> return t
      Just _ -> reportError p ": expected a variable"
      Nothing -> reportError p ": could not find variable"

  variable env (AST.SubscriptVariable var index p) =
    expression env index >>= indexType env
      >> variable env var >>= arrayType env
    where indexType _ Int = return Int
          indexType _ _ = reportError p ": index must be an int"
          arrayType _ (Array t) = return t
          arrayType _ _ = reportError p ": subscripted must be an array"

  variable env (AST.FieldVariable record (member, _) p) =
    variable env record >>= field
    where field :: Type -> Either String Type
          field (Record fs) =
            case lookup member fs of
              Just ty -> return ty
              Nothing -> reportError p ": could not find field"
          field _ = reportError p ": expected a record type"


  resolveType :: Environment -> AST.Identifier -> Either String Type
  resolveType (_, ts) (name, p) =
    case M.lookup name ts of
      Just t -> return t
      Nothing -> reportError p ": could not find type alias"

  declaration :: Environment -> AST.Declaration -> Either String Environment
  declaration env (AST.VariableDeclaration name Nothing init p) =
    expression env init >>= return . insertValue env name . Variable

  declaration env (AST.VariableDeclaration name (Just ty) init p) =
    do initType <- expression env init
       decType <- resolveType env ty
       typesMatch matchError initType decType
       return $ insertValue env name $ Variable initType
    where matchError = reportError p ": declaration types must match"

  declaration env (AST.TypeDeclarationGroup ds) = foldM bind env ds
    where bind :: Environment -> AST.TypeDeclaration -> Either String Environment
          bind env (AST.TypeDeclaration name ty _) =
            actual env ty >>= return . insertType env name
          actual :: Environment -> AST.Type -> Either String Type
          actual env (AST.NamedType name) = resolveType env name
          actual env (AST.ArrayType name p) =
            resolveType env name >>= return . Array
          actual env (AST.RecordType fs p) =
            mapM (field env) fs >>= return . Record
          field env (AST.Field (name, _) ty) =
            resolveType env ty >>= return . (,) name

  declaration _ node = reportError node "\nCan't check declaration"

  typesMatch :: Either String Type -> Type -> Type -> Either String Type
  typesMatch message a b
    | a == b = return a
    | otherwise = message
