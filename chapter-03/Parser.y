{
module Parser(makeParseTree) where

import Tokens
}

%name makeParseTree
%tokentype { Tokens.Token }
%error { parseError }

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
    { () }

BinaryOperator
  : Plus
    { () }
  | Minus
    { () }
  | Times
    { () }
  | Divide
    { () }
  | Equals
    { () }
  | NotEquals
    { () }
  | Less
    { () }
  | Greater
    { () }
  | LessOrEquals
    { () }
  | GreaterOrEquals
    { () }
  | And
    { () }
  | Or
    { () }

Declaration
  : TypeDeclaration
    { () }
  | VariableDeclaration
    { () }
  | FunctionDeclaration
    { () }

DeclarationList
  : Declaration
    { () }
  | DeclarationList Declaration
    { () }

Expression
  : String
    { () }
  | Int
    { () }
  | Nil
    { () }
  | Lvalue
    { () }
  | Identifier
    { () }
  | Minus Expression
    { () }
  | Expression BinaryOperator Expression
    { () }
  | Lvalue Assign Expression
    { () }
  | Identifier Assign Expression
    { () }
  | Identifier LeftParen ExpressionList RightParen
    { () }
  | LeftParen RightParen
    { () }
  | LeftParen ExpressionSequence RightParen
    { () }
  | Identifier BeginRecord EndRecord
    { () }
  | Identifier BeginRecord FieldList EndRecord
    { () }
  | Identifier BeginSubscript Expression EndSubscript Of Expression
    { () }
  | If Expression Then Expression
    { () }
  | If Expression Then Expression Else Expression
    { () }
  | While Expression Do Expression
    { () }
  | For Identifier Assign Expression To Expression Do Expression
    { () }
  | Break
    { () }
  | Let DeclarationList In ExpressionSequence End
    { () }

ExpressionList
  : Expression
    { () }
  | ExpressionList Comma Expression
    { () }

ExpressionSequence
  : Expression
    { () }
  | ExpressionSequence Semicolon Expression
    { () }

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