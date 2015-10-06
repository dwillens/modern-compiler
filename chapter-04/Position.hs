{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Position where
  import Text.PrettyPrint.GenericPretty

  data Position = Position { offset :: Int, line :: Int, col :: Int }
    deriving (Eq, Show, Generic, Out)