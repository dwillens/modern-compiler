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
@comments = "/*" (~$star | $star ~$slash)+ "*/"

@integer = $digit+
@identifier = $letter [_ $letter $digit]+
@string = $quote ~$quote* $quote
scanner :-
  @whitespace           ;
  @comments             ;

  -- Reserved words
  "while"               { \p s -> Tokens.While (pos p) }
  "let"                 { \p s -> Tokens.Let (pos p) }
  "in"                  { \p s -> Tokens.In (pos p) }
  "end"                 { \p s -> Tokens.End (pos p) }
  "var"                 { \p s -> Tokens.Var (pos p) }
  "type"                { \p s -> Tokens.Type (pos p) }
  "array"               { \p s -> Tokens.Array (pos p) }
  "of"                  { \p s -> Tokens.Of (pos p) }

  -- Operators
  ","                   { \p s -> Tokens.Comma (pos p) }
  ":"                   { \p s -> Tokens.Colon (pos p) }
  ";"                   { \p s -> Tokens.Semicolon (pos p) }
  "["                   { \p s -> Tokens.LeftBracket (pos p) }
  "]"                   { \p s -> Tokens.RightBracket (pos p) }
  "{"                   { \p s -> Tokens.LeftBrace (pos p) }
  "}"                   { \p s -> Tokens.RightBrace (pos p) }
  "."                   { \p s -> Tokens.Dot (pos p) }
  "="                   { \p s -> Tokens.Equal (pos p) }
  ":="                  { \p s -> Tokens.Assign (pos p) }

  -- Other
  @integer              { \p s -> Tokens.Int (pos p) $ read $ L.unpack s }
  @string               { \p s -> Tokens.String (pos p) $ stringOf s}
  @identifier           { \p s -> Tokens.Identifier (pos p) $ L.unpack s }
{

stringOf :: L.ByteString -> String
stringOf = L.unpack . L.tail . L.init

pos :: AlexPosn -> Tokens.Position
pos (AlexPn offset line col) = Tokens.Position offset line col

scanTokens :: L.ByteString -> [Tokens.Token]
scanTokens = alexScanTokens

}
