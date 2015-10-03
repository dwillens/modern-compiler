module Tokens where
  data Position = Position { offset :: Int, line :: Int, col :: Int }
    deriving (Eq, Show)

  data Token =
      While Position
    | Let Position
    | In Position
    | End Position
    | Var Position
    | Type Position
    | Array Position
    | Of Position

    | Colon Position
    | LeftBracket Position
    | RightBracket Position
    | Equal Position
    | Assign Position

    | Int Position Integer
    | Identifier Position String

    deriving (Eq, Show)