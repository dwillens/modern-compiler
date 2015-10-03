module Tokens where
  data Position = Position { offset :: Int, line :: Int, col :: Int }
    deriving (Eq, Show)

  data Token = Int Position Integer
             | While Position
    deriving (Eq, Show)