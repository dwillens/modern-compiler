{
module Parser(makeParseTree) where

import Tokens
}

%name makeParseTree
%tokentype { Tokens.Token }
%error { parseError }

%token
  Let                   { Tokens.Let _ }
  If                    { Tokens.If _ }
  Then                  { Tokens.Then _ }
  Else                  { Tokens.Else _ }

  LeftParen             { Tokens.LeftParen _ }
  RightParen            { Tokens.RightParen _ }
  Greater               { Tokens.Greater _ }

  Int                   { Tokens.Int $$ _ }

%%

IfStatement   : If LeftParen Exp RightParen Then Exp Else Exp
                  { "if statement" }

Exp           : Int Greater Int
                  { "greater" }
              | Int
                  { "number" }

{

parseError :: [Tokens.Token] -> a
parseError ts = error $ "Could not parse" ++ show ts

}