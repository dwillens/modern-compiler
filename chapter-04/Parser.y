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

BinaryOperatorExpression
  : Expression Plus Expression
    { AST.UnitExpression }
  | Expression Minus Expression
    { AST.UnitExpression }
  | Expression Times Expression
    { AST.UnitExpression }
  | Expression Divide Expression
    { AST.UnitExpression }
  | Expression Equals Expression
    { AST.UnitExpression }
  | Expression NotEquals Expression
    { AST.UnitExpression }
  | Expression Less Expression
    { AST.UnitExpression }
  | Expression Greater Expression
    { AST.UnitExpression }
  | Expression LessOrEquals Expression
    { AST.UnitExpression }
  | Expression GreaterOrEquals Expression
    { AST.UnitExpression }
  | Expression And Expression
    { AST.UnitExpression }
  | Expression Or Expression
    { AST.UnitExpression }

Declaration
  : TypeDeclaration
    { $1 }
  | VariableDeclaration
    { $1 }
  | FunctionDeclaration
    { AST.FunctionDeclaration }

DeclarationList
  : Declaration
    { [$1] }
  | DeclarationList Declaration
    { $1 ++ [$2] }

Expression
  : String
    { AST.UnitExpression }
  | Int
    { AST.IntegerExpression $1 }
  | Nil
    { AST.UnitExpression }
  | Lvalue
    { AST.UnitExpression }
  | Identifier
    { AST.VariableExpression (AST.SimpleVariable $1) }
  | Minus Expression %prec UnaryMinus
    { AST.UnitExpression }
  | BinaryOperatorExpression
    { $1 }
  | Lvalue Assign Expression
    { AST.UnitExpression }
  | Identifier Assign Expression
    { AST.UnitExpression }
  | Identifier LeftParen ExpressionList RightParen
    { AST.UnitExpression }
  | LeftParen RightParen
    { AST.UnitExpression }
  | LeftParen ExpressionSequence RightParen
    { AST.UnitExpression }
  | Identifier BeginRecord EndRecord
    { AST.UnitExpression }
  | Identifier BeginRecord FieldList EndRecord
    { AST.UnitExpression }
  | Identifier BeginSubscript Expression EndSubscript Of Expression
    { AST.ArrayExpression $1 $3 $6 }
  | If Expression Then Expression
    { AST.UnitExpression }
  | If Expression Then Expression Else Expression
    { AST.UnitExpression }
  | While Expression Do Expression
    { AST.UnitExpression }
  | For Identifier Assign Expression To Expression Do Expression
    { AST.UnitExpression }
  | Break
    { AST.UnitExpression }
  | Let DeclarationList In ExpressionSequence End
    { AST.Let $2 $4 }

ExpressionList
  : Expression
    { () }
  | ExpressionList Comma Expression
    { () }

ExpressionSequence
  : Expression
    { [$1] }
  | ExpressionSequence Semicolon Expression
    { $1 ++ [$3] }

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
    { AST.NamedType }
  | BeginRecord TypeFields EndRecord
    { AST.RecordType }
  | Array Of Identifier
    { AST.ArrayType $3 }

TypeDeclaration
  : Type Identifier Equals Ty
    { AST.TypeDeclaration $2 $4 }

VariableDeclaration
  : Var Identifier Assign Expression
    { AST.VariableDeclaration $2 Nothing $4 }
  | Var Identifier Colon Identifier Assign Expression
    { AST.VariableDeclaration $2 (Just $4) $6 }

{

parseError :: [Tokens.Token] -> a
parseError ts = error $ "Could not parse"

}