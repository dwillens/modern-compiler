{

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Scanner (scanTokens) where

import Tokens
import Data.ByteString.Lazy.Char8 as L
}

%wrapper "posn-bytestring"

$digit = 0-9
$letter = [a-zA-Z]
$quote = "

$star = \*
$slash = \/

@whitespace = $white+
-- TODO: Recognize nested and multi-line comments.
@comments = "/*" (~$star | $star ~$slash)+ "*/"

@integer = $digit+
@identifier = $letter [_ $letter $digit]*
-- TODO: Recognize and interpret escape sequences.
@string = $quote ~$quote* $quote

scanner :-
  @whitespace           ;
  @comments             ;

  -- Reserved words
  "while"               { \p s -> Tokens.While $ pos p }
  "for"                 { \p s -> Tokens.For $ pos p }
  "to"                  { \p s -> Tokens.To $ pos p }
  "break"               { \p s -> Tokens.Break $ pos p }
  "let"                 { \p s -> Tokens.Let $ pos p }
  "in"                  { \p s -> Tokens.In $ pos p }
  "end"                 { \p s -> Tokens.End $ pos p }
  "function"            { \p s -> Tokens.Function $ pos p }
  "var"                 { \p s -> Tokens.Var $ pos p }
  "type"                { \p s -> Tokens.Type $ pos p }
  "array"               { \p s -> Tokens.Array $ pos p }
  "if"                  { \p s -> Tokens.If $ pos p }
  "then"                { \p s -> Tokens.Then $ pos p }
  "else"                { \p s -> Tokens.Else $ pos p }
  "do"                  { \p s -> Tokens.Do $ pos p }
  "of"                  { \p s -> Tokens.Of $ pos p }
  "nil"                 { \p s -> Tokens.Nil $ pos p }

  -- Operators
  ","                   { \p s -> Tokens.Comma $ pos p }
  ":"                   { \p s -> Tokens.Colon $ pos p }
  ";"                   { \p s -> Tokens.Semicolon $ pos p }
  "("                   { \p s -> Tokens.LeftParen $ pos p }
  ")"                   { \p s -> Tokens.RightParen $ pos p }
  "["                   { \p s -> Tokens.BeginSubscript $ pos p }
  "]"                   { \p s -> Tokens.EndSubscript $ pos p }
  "{"                   { \p s -> Tokens.BeginRecord $ pos p }
  "}"                   { \p s -> Tokens.EndRecord $ pos p }
  "."                   { \p s -> Tokens.Member $ pos p }
  "+"                   { \p s -> Tokens.Plus $ pos p }
  "-"                   { \p s -> Tokens.Minus $ pos p }
  "*"                   { \p s -> Tokens.Times $ pos p }
  "/"                   { \p s -> Tokens.Divide $ pos p }
  "="                   { \p s -> Tokens.Equals $ pos p }
  "<>"                  { \p s -> Tokens.NotEquals $ pos p }
  "<"                   { \p s -> Tokens.Less $ pos p }
  "<="                  { \p s -> Tokens.LessOrEquals $ pos p }
  ">"                   { \p s -> Tokens.Greater $ pos p }
  ">="                  { \p s -> Tokens.GreaterOrEquals $ pos p }
  "&"                   { \p s -> Tokens.And $ pos p }
  "|"                   { \p s -> Tokens.Or $ pos p }
  ":="                  { \p s -> Tokens.Assign $ pos p }

  -- Other
  @integer              { \p s -> Tokens.Int (read $ L.unpack s) $ pos p }
  @string               { \p s -> Tokens.String (stringOf s) $ pos p }
  @identifier           { \p s -> Tokens.Identifier (L.unpack s) $ pos p }
{

stringOf :: L.ByteString -> String
stringOf = L.unpack . L.tail . L.init

pos :: AlexPosn -> Tokens.Position
pos (AlexPn offset line col) = Tokens.Position offset line col

scanTokens :: L.ByteString -> [Tokens.Token]
scanTokens = alexScanTokens

}
