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

  typeCheck :: AST.Expression -> Either String ()
  typeCheck = void . expression (M.empty, standardTypes)
    where standardTypes = M.fromList [("int", Int), ("string", String)]

  reportError :: Show p => p -> String -> Either String a
  reportError p = Left . (++) (show p)

  insertValue :: Environment -> AST.Identifier -> Value -> Environment
  insertValue (vs, ts) (name, _) val = (M.insert name val vs, ts)

  insertType :: Environment -> AST.Identifier -> Type -> Environment
  insertType (vs, ts) (name, _) ty = (vs, M.insert name ty ts)


  typeMatch :: Either String Type -> Type -> Type -> Either String Type
  typeMatch _ r@(Record _) Nil = return r
  typeMatch _ Nil r@(Record _) = return r
  typeMatch message a b
    | a == b = return a
    | otherwise = message

  lengthsMatch :: Either String () -> [a] -> [b] -> Either String ()
  lengthsMatch message inits fs
    | length inits == length fs = return ()
    | otherwise = message


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


  function :: Environment -> AST.Identifier -> Either String ([Type], Type)
  function (vs, _) (name, p) =
    case M.lookup name vs of
      Just (Function params result) -> return (params, result)
      Nothing -> reportError p ": expected a function"


  expression :: Environment -> AST.Expression -> Either String Type
  expression env (AST.VariableExpression v) = variable env v

  expression _ (AST.NilExpression _) = return Nil
  expression _ (AST.IntegerExpression _ _) = return Int
  expression _ (AST.StringExpression _ _) = return String

  expression env (AST.CallExpression f args p) =
    do (paramTypes, resultType) <- function env f
       lengthsMatch lengthError paramTypes args
       argTypes <- forM args $ expression env
       zipWithM (typeMatch matchError) paramTypes argTypes
       return resultType
    where matchError = reportError p ": argument types do not match"
          lengthError = reportError p ": wrong number of arguments"

  expression env (AST.ArithmeticExpression _ l r p) =
    expression env l >>= operandType env
      >> expression env r >>= operandType env
    where operandType _ Int = return Int
          operandType _ _ = reportError p ": operand must be int"

  expression env (AST.EqualityExpression _ l r p) =
    do lType <- expression env l
       rType <- expression env r
       typeMatch matchError lType rType
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
    resolveType env ty >>= recordType env fs
    where recordType :: Environment -> [(AST.Identifier, AST.Expression)] -> Type -> Either String Type
          recordType env inits rec@(Record fs) =
            do lengthsMatch lengthError inits fs
               zipWithM (fieldType env) fs inits
               return rec
          recordType _ _ _ = reportError p ": expected record type"
          fieldType :: Environment -> (String, Type) -> (AST.Identifier, AST.Expression) -> Either String Type
          fieldType env (f, ty) ((name, _), init)
            | f == name = expression env init >>= typeMatch matchError ty
            | otherwise = reportError p ": wrong field"
          matchError = reportError p ": field init type must match"
          lengthError = reportError p ": wrong number of fields"

  expression env (AST.AssignExpression to val p) =
    do toType <- variable env to
       valType <- expression env val
       typeMatch matchError toType valType
       return Unit
    where matchError = reportError p ": assignment types must match"

  expression env (AST.IfExpression test thenE (Just elseE) p) =
    expression env test >>= testType env
      >> do thenType <- expression env thenE
            elseType <- expression env elseE
            typeMatch matchError thenType elseType
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




  resolveType :: Environment -> AST.Identifier -> Either String Type
  resolveType (_, ts) (name, p) =
    case M.lookup name ts of
      Just t -> return t
      Nothing -> reportError p ": could not find type alias"

  declaration :: Environment -> AST.Declaration -> Either String Environment
  declaration env (AST.VariableDeclaration name Nothing init p) =
    expression env init >>= initType env
      >>= return . insertValue env name . Variable
    where initType _ Unit = reportError p ": cannot assign unit"
          initType _ Nil = reportError p ": cannot assign nil"
          initType _ t = return t

  declaration env (AST.VariableDeclaration name (Just ty) init p) =
    do initType <- expression env init
       decType <- resolveType env ty
       typeMatch matchError initType decType
       return $ insertValue env name $ Variable initType
    where matchError = reportError p ": declaration types must match"

  declaration env (AST.TypeDeclarationGroup ds) = foldM bindType env ds

  declaration env (AST.FunctionDeclarationGroup ds) = foldM bindFunction env ds

  bindType :: Environment -> AST.TypeDeclaration -> Either String Environment
  bindType env (AST.TypeDeclaration name ty _) =
    actual env ty >>= return . insertType env name
    where actual :: Environment -> AST.Type -> Either String Type
          actual env (AST.NamedType name) = resolveType env name
          actual env (AST.ArrayType name p) =
            resolveType env name >>= return . Array
          actual env (AST.RecordType fs p) =
            forM fs (field env) >>= return . Record
          field env (AST.Field (name, _) ty) =
            resolveType env ty >>= return . (,) name

  bindFunction :: Environment -> AST.FunctionDeclaration -> Either String Environment
  bindFunction env (AST.FunctionDeclaration name params result body p) =
    do resultType <- resolveResultType env result
       formals <- forM params $ field env
       let env' = insertValue env name $ Function (map snd formals) resultType
       let env'' = foldl insertField env' formals
       bodyType <- expression env'' body
       typeMatch matchError bodyType resultType
       return env'
    where insertField env (name, ty) = insertValue env name $ Variable ty
          matchError = reportError p ": body and result type must match"
          field env (AST.Field name ty) = resolveType env ty >>= return . (,) name
          resolveResultType env (Just t) = resolveType env t
          resolveResultType _ Nothing = return Unit
