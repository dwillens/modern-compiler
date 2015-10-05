{

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Scanner (getToken
               ,runScanner
               ,reportError
               ,listTokens
               ,Scanner(..)
               ) where

import Position
import Tokens

import GHC.Int
import qualified Data.ByteString.Lazy.Char8 as L

import Control.Monad(ap, liftM)
import Control.Monad.Loops(untilM)

}

%wrapper "monad-bytestring"

$digit = 0-9
$letter = [a-zA-Z]
$quote = "

$star = \*
$slash = \/

@whitespace = $white+
-- TODO: Recognize nested comments.
@comments = "/*" (~$star | $star ~$slash | $white)+ "*/"

@integer = $digit+
@identifier = $letter [_ $letter $digit]*
-- TODO: Recognize and interpret escape sequences.
@string = $quote ~$quote* $quote

scanner :-
  @whitespace           ;
  @comments             ;

  -- Reserved words
  "while"               { \a _ -> return $ Tokens.While $ pos a }
  "for"                 { \a _ -> return $ Tokens.For $ pos a }
  "to"                  { \a _ -> return $ Tokens.To $ pos a }
  "break"               { \a _ -> return $ Tokens.Break $ pos a }
  "let"                 { \a _ -> return $ Tokens.Let $ pos a }
  "in"                  { \a _ -> return $ Tokens.In $ pos a }
  "end"                 { \a _ -> return $ Tokens.End $ pos a }
  "function"            { \a _ -> return $ Tokens.Function $ pos a }
  "var"                 { \a _ -> return $ Tokens.Var $ pos a }
  "type"                { \a _ -> return $ Tokens.Type $ pos a }
  "array"               { \a _ -> return $ Tokens.Array $ pos a }
  "if"                  { \a _ -> return $ Tokens.If $ pos a }
  "then"                { \a _ -> return $ Tokens.Then $ pos a }
  "else"                { \a _ -> return $ Tokens.Else $ pos a }
  "do"                  { \a _ -> return $ Tokens.Do $ pos a }
  "of"                  { \a _ -> return $ Tokens.Of $ pos a }
  "nil"                 { \a _ -> return $ Tokens.Nil $ pos a }

  -- Operators
  ","                   { \a _ -> return $ Tokens.Comma $ pos a }
  ":"                   { \a _ -> return $ Tokens.Colon $ pos a }
  ";"                   { \a _ -> return $ Tokens.Semicolon $ pos a }
  "("                   { \a _ -> return $ Tokens.LeftParen $ pos a }
  ")"                   { \a _ -> return $ Tokens.RightParen $ pos a }
  "["                   { \a _ -> return $ Tokens.BeginSubscript $ pos a }
  "]"                   { \a _ -> return $ Tokens.EndSubscript $ pos a }
  "{"                   { \a _ -> return $ Tokens.BeginRecord $ pos a }
  "}"                   { \a _ -> return $ Tokens.EndRecord $ pos a }
  "."                   { \a _ -> return $ Tokens.Member $ pos a }
  "+"                   { \a _ -> return $ Tokens.Plus $ pos a }
  "-"                   { \a _ -> return $ Tokens.Minus $ pos a }
  "*"                   { \a _ -> return $ Tokens.Times $ pos a }
  "/"                   { \a _ -> return $ Tokens.Divide $ pos a }
  "="                   { \a _ -> return $ Tokens.Equals $ pos a }
  "<>"                  { \a _ -> return $ Tokens.NotEquals $ pos a }
  "<"                   { \a _ -> return $ Tokens.Less $ pos a }
  "<="                  { \a _ -> return $ Tokens.LessOrEquals $ pos a }
  ">"                   { \a _ -> return $ Tokens.Greater $ pos a }
  ">="                  { \a _ -> return $ Tokens.GreaterOrEquals $ pos a }
  "&"                   { \a _ -> return $ Tokens.And $ pos a }
  "|"                   { \a _ -> return $ Tokens.Or $ pos a }
  ":="                  { \a _ -> return $ Tokens.Assign $ pos a }

  -- Other
  @integer              { \a n -> return $ Tokens.Int (intOf a n)  $ pos a }
  @string               { \a n -> return $ Tokens.String (stringOf a n) $ pos a }
  @identifier           { \a n -> return $ Tokens.Identifier (identifierOf a n) $ pos a }
{

pos :: AlexInput -> Position
pos (AlexPn offset line col, _, _) = Position offset line col


intOf :: AlexInput -> Int64 -> Integer
intOf (_, _, s) n = read $ L.unpack $ L.take n s

stringOf :: AlexInput -> Int64 -> String
stringOf (_, _, s) n = L.unpack $ L.take (n - 2) $ L.tail s

identifierOf :: AlexInput -> Int64 -> String
identifierOf (_, _, s) n = L.unpack $ L.take n s


listTokens :: L.ByteString -> Either String [Tokens.Token]
listTokens input = runAlex input $ getToken collect
  where collect :: Tokens.Token -> Alex [Tokens.Token]
        collect Tokens.EOF = return []
        collect token = getToken collect >>= return . (token :)

runScanner :: L.ByteString -> Alex a -> Either String a
runScanner = runAlex

getToken :: (Tokens.Token -> Alex a) -> Alex a
getToken = (alexMonadScan >>=)

reportError :: String -> Alex a
reportError = alexError


alexEOF :: Alex Tokens.Token
alexEOF = return Tokens.EOF

type Scanner a = Alex a

instance Functor Alex where
  fmap = liftM

instance Applicative Alex where
  pure = return
  (<*>) = ap
}
