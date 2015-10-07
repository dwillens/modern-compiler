{
module Parser(runParser) where

import qualified Data.ByteString.Lazy as L

import qualified Scanner
import qualified Tokens
import qualified AbstractSyntaxTree as AST
}
-- TODO Add error handling.

%name parse
%tokentype { Tokens.Token }
%error { parseError }
%monad { Scanner.Scanner }
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
  While                 { Tokens.While $$ }
  For                   { Tokens.For $$ }
  To                    { Tokens.To $$ }
  Break                 { Tokens.Break $$ }
  Let                   { Tokens.Let $$ }
  In                    { Tokens.In $$ }
  End                   { Tokens.End $$ }
  Function              { Tokens.Function $$ }
  Var                   { Tokens.Var $$ }
  Type                  { Tokens.Type $$ }
  Array                 { Tokens.Array $$ }
  If                    { Tokens.If $$ }
  Then                  { Tokens.Then $$ }
  Else                  { Tokens.Else $$ }
  Do                    { Tokens.Do $$ }
  Of                    { Tokens.Of $$ }
  Nil                   { Tokens.Nil $$ }

  Comma                 { Tokens.Comma $$ }
  Colon                 { Tokens.Colon $$ }
  Semicolon             { Tokens.Semicolon $$ }
  LeftParen             { Tokens.LeftParen $$ }
  RightParen            { Tokens.RightParen $$ }
  BeginSubscript        { Tokens.BeginSubscript $$ }
  EndSubscript          { Tokens.EndSubscript $$ }
  BeginRecord           { Tokens.BeginRecord $$ }
  EndRecord             { Tokens.EndRecord $$ }
  Member                { Tokens.Member $$ }
  Minus                 { Tokens.Minus $$ }
  Plus                  { Tokens.Plus $$ }
  Times                 { Tokens.Times $$ }
  Divide                { Tokens.Divide $$ }
  Equals                { Tokens.Equals $$ }
  NotEquals             { Tokens.NotEquals $$ }
  Less                  { Tokens.Less $$ }
  LessOrEquals          { Tokens.LessOrEquals $$ }
  Greater               { Tokens.Greater $$ }
  GreaterOrEquals       { Tokens.GreaterOrEquals $$ }
  And                   { Tokens.And $$ }
  Or                    { Tokens.Or $$ }
  Assign                { Tokens.Assign $$ }


  Int                   { Tokens.Int $$ }
  String                { Tokens.String $$ }
  Identifier            { Tokens.Identifier $$ }

%%
Program
  : Expression
    { $1 }

BinaryOperatorExpression
  : Expression Plus Expression
    { AST.ArithmeticExpression AST.Add $1 $3 $2 }
  | Expression Minus Expression
    { AST.ArithmeticExpression AST.Subtract $1 $3 $2 }
  | Expression Times Expression
    { AST.ArithmeticExpression AST.Multiply $1 $3 $2 }
  | Expression Divide Expression
    { AST.ArithmeticExpression AST.Divide $1 $3 $2 }

  | Expression Equals Expression
    { AST.ComparisonExpression AST.Equals $1 $3 $2 }
  | Expression NotEquals Expression
    { AST.ComparisonExpression AST.NotEquals $1 $3 $2 }
  | Expression Less Expression
    { AST.ComparisonExpression AST.Less $1 $3 $2 }
  | Expression Greater Expression
    { AST.ComparisonExpression AST.Greater $1 $3 $2 }
  | Expression LessOrEquals Expression
    { AST.ComparisonExpression AST.LessOrEquals $1 $3 $2 }
  | Expression GreaterOrEquals Expression
    { AST.ComparisonExpression AST.GreaterOrEquals $1 $3 $2 }

  | Expression And Expression
    { AST.IfExpression $1 $3 (Just $ AST.IntegerExpression 0 $2) $2 }
  | Expression Or Expression
    { AST.IfExpression $1 (AST.IntegerExpression 1 $2) (Just $3) $2 }

Declaration
  : TypeDeclarationGroup      { AST.TypeDeclarationGroup (reverse $1) }
  | VariableDeclaration       { $1 }
  | FunctionDeclarationGroup  { AST.FunctionDeclarationGroup (reverse $1) }

DeclarationList
  : Declaration                  { [$1] }
  | DeclarationList Declaration  { $2 : $1 }

Expression
  : String  { let (s, p) = $1 in AST.StringExpression s p }
  | Int     { let (i, p) = $1 in AST.IntegerExpression i p }
  | Nil     { AST.NilExpression $1 }

  | Lvalue              { AST.VariableExpression $1 }
  | Identifier          { AST.VariableExpression (AST.SimpleVariable $1) }

  | Minus Expression %prec UnaryMinus
    { AST.ArithmeticExpression AST.Subtract (AST.IntegerExpression 0 $1) $2 $1 }

  | BinaryOperatorExpression { $1 }

  | Lvalue Assign Expression      { AST.AssignExpression $1 $3 $2 }
  | Identifier Assign Expression  { AST.AssignExpression (AST.SimpleVariable $1) $3 $2 }

  | Identifier LeftParen RightParen { AST.CallExpression $1 [] $2 }
  | Identifier LeftParen ExpressionList RightParen { AST.CallExpression $1 (reverse $3) $2 }

  | LeftParen RightParen                    { AST.SequenceExpression [] $1 }
  | LeftParen ExpressionSequence RightParen { AST.SequenceExpression (reverse $2) $1 }

  | Identifier BeginRecord EndRecord            { AST.RecordExpression $1 [] $2 }
  | Identifier BeginRecord FieldList EndRecord  { AST.RecordExpression $1 (reverse $3) $2 }

  | Identifier BeginSubscript Expression EndSubscript Of Expression
    { AST.ArrayExpression $1 $3 $6 $2 }

  | If Expression Then Expression                 { AST.IfExpression $2 $4 Nothing $1 }
  | If Expression Then Expression Else Expression { AST.IfExpression $2 $4 (Just $6) $1 }

  | While Expression Do Expression { AST.WhileExpression $2 $4 $1 }

  | For Identifier Assign Expression To Expression Do Expression
    { AST.ForExpression $2 $4 $6 $8 $1 }

  | Break { AST.BreakExpression $1 }

  | Let DeclarationList In ExpressionSequence End
    { AST.LetExpression (reverse $2) (reverse $4) $1 }

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
  : Function Identifier LeftParen RightParen Equals Expression
    { AST.FunctionDeclaration $2 [] Nothing $6 $1 }
  | Function Identifier LeftParen TypeFields RightParen Equals Expression
    { AST.FunctionDeclaration $2 (reverse $4) Nothing $7 $1 }

  | Function Identifier LeftParen RightParen Colon Identifier Equals Expression
    { AST.FunctionDeclaration $2 [] (Just $6) $8 $1 }
  | Function Identifier LeftParen TypeFields RightParen Colon Identifier Equals Expression
    { AST.FunctionDeclaration $2 (reverse $4) (Just $7) $9 $1 }

-- TODO Figure out how to resolve shift/reduce conflict here.
FunctionDeclarationGroup
  : FunctionDeclaration                           { [$1] }
  | FunctionDeclarationGroup FunctionDeclaration  { $2 : $1 }

Lvalue
  : Lvalue Member Identifier
    { AST.FieldVariable $1 $3 $2 }
  | Identifier Member Identifier
    { AST.FieldVariable (AST.SimpleVariable $1) $3 $2 }
  | Lvalue BeginSubscript Expression EndSubscript
    { AST.SubscriptVariable $1 $3 $2 }
  | Identifier BeginSubscript Expression EndSubscript
    { AST.SubscriptVariable (AST.SimpleVariable $1) $3 $2 }

TypeField
  : Identifier Colon Identifier { AST.Field $1 $3 }

TypeFields
  : TypeField                   { [$1] }
  | TypeFields Comma TypeField  { $3 : $1 }

Ty
  : Identifier                        { AST.NamedType $1 }
  | BeginRecord TypeFields EndRecord  { AST.RecordType (reverse $2) $1 }
  | Array Of Identifier               { AST.ArrayType $3 $1 }

TypeDeclaration
  : Type Identifier Equals Ty { AST.TypeDeclaration $2 $4 $1 }

-- TODO Figure out how to resolve shift/reduce conflict here.
TypeDeclarationGroup
  : TypeDeclaration                       { [$1] }
  | TypeDeclarationGroup TypeDeclaration  { $2 : $1 }

VariableDeclaration
  : Var Identifier Assign Expression
    { AST.VariableDeclaration $2 Nothing $4 $1 }
  | Var Identifier Colon Identifier Assign Expression
    { AST.VariableDeclaration $2 (Just $4) $6 $1 }

{

runParser :: L.ByteString -> Either String AST.Expression
runParser input = Scanner.runScanner input parse

reportError :: String -> Scanner.Scanner a
reportError = Scanner.reportError

parseError :: Tokens.Token -> Scanner.Scanner a
parseError token = reportError ("Could not parse at " ++ show token)

}