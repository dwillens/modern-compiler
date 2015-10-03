{

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Scanner (scanTokens) where

import Tokens
import Data.ByteString.Lazy.Char8 as L
}

%wrapper "basic-bytestring"

$digit = 0-9
$whitespace = [\t\n ]

scanner :-
  $white+               ;
  $digit+               { \s -> Tokens.Int $ read $ L.unpack s }

{

scanTokens = alexScanTokens
}
