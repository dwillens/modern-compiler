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

    | Comma Position
    | Colon Position
    | Semicolon Position
    | BeginSubscript Position
    | EndSubscript Position
    | BeginRecord Position
    | EndRecord Position
    | Member Position
    | Equals Position
    | Assign Position

    | Int Position Integer
    | String Position String
    | Identifier Position String

    deriving (Eq, Show)