{

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Scanner (scanTokens) where

import Tokens
import Data.ByteString.Lazy.Char8 as L
}

%wrapper "posn-bytestring"

$digit = 0-9
$whitespace = [\t\n ]

scanner :-
  $white+               ;
  $digit+               { \p s -> Tokens.Int (pos p) $ read $ L.unpack s }

{

pos :: AlexPosn -> Tokens.Position
pos (AlexPn offset line col) = Tokens.Position offset line col

scanTokens :: L.ByteString -> [Tokens.Token]
scanTokens = alexScanTokens

}
