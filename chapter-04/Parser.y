{
module Parser(runParser) where

import qualified Data.ByteString.Lazy.Char8 as L

import qualified Scanner
import qualified Tokens
import qualified AbstractSyntaxTree as AST
}
-- TODO Add error handling.

%name parse
%tokentype { Tokens.Token }
%error { parseError }
%monad { Scanner.Alex }
%lexer { Scanner.getToken } { Tokens.EOF }

%nonassoc Then Do Of
%nonassoc Else
%left Assign
%left Or
%left And
%nonassoc Equals NotEquals Less Greater LessOrEquals GreaterOrEquals
%left Plus Minus
%left Times Divide
%nonassoc UnaryMinus

%token
  While                 { Tokens.While _ }
  For                   { Tokens.For _ }
  To                    { Tokens.To _ }
  Break                 { Tokens.Break _ }
  Let                   { Tokens.Let _ }
  In                    { Tokens.In _ }
  End                   { Tokens.End _ }
  Function              { Tokens.Function _ }
  Var                   { Tokens.Var _ }
  Type                  { Tokens.Type _ }
  Array                 { Tokens.Array _ }
  If                    { Tokens.If _ }
  Then                  { Tokens.Then _ }
  Else                  { Tokens.Else _ }
  Do                    { Tokens.Do _ }
  Of                    { Tokens.Of _ }
  Nil                   { Tokens.Nil _ }

  Comma                 { Tokens.Comma _ }
  Colon                 { Tokens.Colon _ }
  Semicolon             { Tokens.Semicolon _ }
  LeftParen             { Tokens.LeftParen _ }
  RightParen            { Tokens.RightParen _ }
  BeginSubscript        { Tokens.BeginSubscript _ }
  EndSubscript          { Tokens.EndSubscript _ }
  BeginRecord           { Tokens.BeginRecord _ }
  EndRecord             { Tokens.EndRecord _ }
  Member                { Tokens.Member _ }
  Minus                 { Tokens.Minus _ }
  Plus                  { Tokens.Plus _ }
  Times                 { Tokens.Times _ }
  Divide                { Tokens.Divide _ }
  Equals                { Tokens.Equals _ }
  NotEquals             { Tokens.NotEquals _ }
  Less                  { Tokens.Less _ }
  LessOrEquals          { Tokens.LessOrEquals _ }
  Greater               { Tokens.Greater _ }
  GreaterOrEquals       { Tokens.GreaterOrEquals _ }
  And                   { Tokens.And _ }
  Or                    { Tokens.Or _ }
  Assign                { Tokens.Assign _ }


  Int                   { Tokens.Int $$ _ }
  String                { Tokens.String $$ _ }
  Identifier            { Tokens.Identifier $$ _ }

%%
Program
  : Expression
    { $1 }

BinaryOperatorExpression
  : Expression Plus Expression
    { AST.ArithmeticExpression AST.Add $1 $3}
  | Expression Minus Expression
    { AST.ArithmeticExpression AST.Subtract $1 $3 }
  | Expression Times Expression
    { AST.ArithmeticExpression AST.Multiply $1 $3 }
  | Expression Divide Expression
    { AST.ArithmeticExpression AST.Divide $1 $3 }

  | Expression Equals Expression
    { AST.ComparisonExpression AST.Equals $1 $3 }
  | Expression NotEquals Expression
    { AST.ComparisonExpression AST.NotEquals $1 $3 }
  | Expression Less Expression
    { AST.ComparisonExpression AST.Less $1 $3 }
  | Expression Greater Expression
    { AST.ComparisonExpression AST.Greater $1 $3 }
  | Expression LessOrEquals Expression
    { AST.ComparisonExpression AST.LessOrEquals $1 $3 }
  | Expression GreaterOrEquals Expression
    { AST.ComparisonExpression AST.GreaterOrEquals $1 $3 }

  | Expression And Expression
    { AST.IfExpression $1 $3 (Just $ AST.IntegerExpression 0) }
  | Expression Or Expression
    { AST.IfExpression $1 (AST.IntegerExpression 1) (Just $3) }

DeclarationGroup
  : TypeDeclarationGroup      { AST.TypeDeclarationGroup (reverse $1) }
  | VariableDeclaration       { $1 }
  | FunctionDeclarationGroup  { AST.FunctionDeclarationGroup (reverse $1) }

DeclarationList
  : DeclarationGroup                  { [$1] }
  | DeclarationList DeclarationGroup  { $2 : $1 }

Expression
  : String  { AST.StringExpression $1 }
  | Int     { AST.IntegerExpression $1 }
  | Nil     { AST.NilExpression }

  | Lvalue              { AST.VariableExpression $1 }
  | Identifier          { AST.VariableExpression (AST.SimpleVariable $1) }

  | Minus Expression %prec UnaryMinus
    { AST.ArithmeticExpression AST.Subtract (AST.IntegerExpression 0) $2 }

  | BinaryOperatorExpression { $1 }

  | Lvalue Assign Expression      { AST.AssignExpression $1 $3 }
  | Identifier Assign Expression  { AST.AssignExpression (AST.SimpleVariable $1) $3 }

  | Identifier LeftParen ExpressionList RightParen { AST.CallExpression $1 (reverse $3) }

  | LeftParen RightParen                    { AST.SequenceExpression [] }
  | LeftParen ExpressionSequence RightParen { AST.SequenceExpression (reverse $2) }

  | Identifier BeginRecord EndRecord            { AST.RecordExpression $1 [] }
  | Identifier BeginRecord FieldList EndRecord  { AST.RecordExpression $1 (reverse $3) }

  | Identifier BeginSubscript Expression EndSubscript Of Expression
    { AST.ArrayExpression $1 $3 $6 }

  | If Expression Then Expression                 { AST.IfExpression $2 $4 Nothing }
  | If Expression Then Expression Else Expression { AST.IfExpression $2 $4 (Just $6) }

  | While Expression Do Expression { AST.WhileExpression $2 $4 }

  | For Identifier Assign Expression To Expression Do Expression
    { AST.ForExpression $2 $4 $6 $8 }

  | Break { AST.BreakExpression }

  | Let DeclarationList In ExpressionSequence End
    { AST.LetExpression (reverse $2) (reverse $4) }

ExpressionList
  : Expression                      { [$1] }
  | ExpressionList Comma Expression { $3 : $1 }

ExpressionSequence
  : Expression                                { [$1] }
  | ExpressionSequence Semicolon Expression   { $3 : $1 }

FieldList
  : Identifier Equals Expression                  { [($1, $3)] }
  | FieldList Comma Identifier Equals Expression  { ($3, $5) : $1 }

FunctionDeclaration
  : Function Identifier LeftParen TypeFields RightParen Equals Expression
    { AST.FunctionDeclaration $2 (reverse $4) Nothing $7 }
  | Function Identifier LeftParen TypeFields RightParen Colon Identifier Equals Expression
    { AST.FunctionDeclaration $2 (reverse $4) (Just $7) $9 }

-- TODO Figure out how to resolve shift/reduce conflict here.
FunctionDeclarationGroup
  : FunctionDeclaration                           { [$1] }
  | FunctionDeclarationGroup FunctionDeclaration  { $2 : $1 }

Lvalue
  : Lvalue Member Identifier
    { AST.FieldVariable $1 $3 }
  | Identifier Member Identifier
    { AST.FieldVariable (AST.SimpleVariable $1) $3 }
  | Lvalue BeginSubscript Expression EndSubscript
    { AST.SubscriptVariable $1 $3 }
  | Identifier BeginSubscript Expression EndSubscript
    { AST.SubscriptVariable (AST.SimpleVariable $1) $3 }

TypeField
  : Identifier Colon Identifier { AST.Field $1 $3 }

TypeFields
  : TypeField                   { [$1] }
  | TypeFields Comma TypeField  { $3 : $1 }

Ty
  : Identifier                        { AST.NamedType $1 }
  | BeginRecord TypeFields EndRecord  { AST.RecordType (reverse $2) }
  | Array Of Identifier               { AST.ArrayType $3 }

TypeDeclaration
  : Type Identifier Equals Ty { AST.TypeDeclaration $2 $4 }

-- TODO Figure out how to resolve shift/reduce conflict here.
TypeDeclarationGroup
  : TypeDeclaration                       { [$1] }
  | TypeDeclarationGroup TypeDeclaration  { $2 : $1 }

VariableDeclaration
  : Var Identifier Assign Expression
    { AST.VariableDeclaration $2 Nothing $4 }
  | Var Identifier Colon Identifier Assign Expression
    { AST.VariableDeclaration $2 (Just $4) $6 }

{

runParser :: L.ByteString -> Either String AST.Expression
runParser input = Scanner.runScanner input parse

reportError :: String -> Scanner.Alex a
reportError = Scanner.reportError

parseError :: Tokens.Token -> Scanner.Alex a
parseError token =
  reportError ("Could not parse at " ++ show token)

}