{
module Parser(makeParseTree) where

import qualified Tokens
import qualified AbstractSyntaxTree as AST
}
-- TODO Add error handling.
-- TODO Make the parser/lexer monadic so parse errors can have line numbers.

%name makeParseTree
%tokentype { Tokens.Token }
%error { parseError }

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

Declaration
  : TypeDeclaration
    { () }
  | VariableDeclaration
    { () }
  | FunctionDeclaration
    { () }

DeclarationList
  : Declaration
    { [] }
  | DeclarationList Declaration
    { [] }

Expression
  : String
    { AST.Unit }
  | Int
    { AST.Unit }
  | Nil
    { AST.Unit }
  | Lvalue
    { AST.Unit }
  | Identifier
    { AST.VarExp $ AST.SimpleVar $1 }
  | Minus Expression %prec UnaryMinus
    { AST.Unit }
  | Expression Plus Expression
    { AST.Unit }
  | Expression Minus Expression
    { AST.Unit }
  | Expression Times Expression
    { AST.Unit }
  | Expression Divide Expression
    { AST.Unit }
  | Expression Equals Expression
    { AST.Unit }
  | Expression NotEquals Expression
    { AST.Unit }
  | Expression Less Expression
    { AST.Unit }
  | Expression Greater Expression
    { AST.Unit }
  | Expression LessOrEquals Expression
    { AST.Unit }
  | Expression GreaterOrEquals Expression
    { AST.Unit }
  | Expression And Expression
    { AST.Unit }
  | Expression Or Expression
    { AST.Unit }
  | Lvalue Assign Expression
    { AST.Unit }
  | Identifier Assign Expression
    { AST.Unit }
  | Identifier LeftParen ExpressionList RightParen
    { AST.Unit }
  | LeftParen RightParen
    { AST.Unit }
  | LeftParen ExpressionSequence RightParen
    { AST.Unit }
  | Identifier BeginRecord EndRecord
    { AST.Unit }
  | Identifier BeginRecord FieldList EndRecord
    { AST.Unit }
  | Identifier BeginSubscript Expression EndSubscript Of Expression
    { AST.Unit }
  | If Expression Then Expression
    { AST.Unit }
  | If Expression Then Expression Else Expression
    { AST.Unit }
  | While Expression Do Expression
    { AST.Unit }
  | For Identifier Assign Expression To Expression Do Expression
    { AST.Unit }
  | Break
    { AST.Unit }
  | Let DeclarationList In ExpressionSequence End
    { AST.Let $2 $4 }

ExpressionList
  : Expression
    { () }
  | ExpressionList Comma Expression
    { () }

ExpressionSequence
  : Expression
    { [] }
  | ExpressionSequence Semicolon Expression
    { [] }

FieldList
  : Identifier Equals Expression
    { () }
  | FieldList Comma Identifier Equals Expression
    { () }

FunctionDeclaration
  : Function Identifier LeftParen TypeFields RightParen Equals Expression
    { () }
  | Function Identifier LeftParen TypeFields RightParen Colon Identifier Equals Expression
    { () }

Lvalue
  : Lvalue Member Identifier
    { () }
  | Identifier Member Identifier
    { () }
  | Lvalue BeginSubscript Expression EndSubscript
    { () }
  | Identifier BeginSubscript Expression EndSubscript
    { () }

TypeField
  : Identifier Colon Identifier
    { () }

TypeFields
  : TypeField
    { () }
  | TypeFields Comma TypeField
    { () }

Ty
  : Identifier
    { () }
  | BeginRecord TypeFields EndRecord
    { () }
  | Array Of Identifier
    { () }

TypeDeclaration
  : Type Identifier Equals Ty
    { () }

VariableDeclaration
  : Var Identifier Assign Expression
    { () }
  | Var Identifier Colon Identifier Assign Expression
    { () }

{

parseError :: [Tokens.Token] -> a
parseError ts = error $ "Could not parse"

}