{
module Scanner (scan) where
}

%wrapper "basic"

$digit = 0-9
$whitespace = [\t\n ]

scanner :-
  $white+               ;
  $digit+               { \s -> TokenInt $ read s }


