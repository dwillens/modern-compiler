module Position where
  data Position = Position { offset :: Int, line :: Int, col :: Int }
    deriving (Eq, Show)