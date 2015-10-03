module Tokens where
  data Position = Position { offset :: Int, line :: Int, col :: Int }
    deriving (Eq, Show)

  data Token =
      While Position
    | Let Position
    | In Position
    | End Position
    | Function Position
    | Var Position
    | Type Position
    | Array Position
    | If Position
    | Then Position
    | Else Position
    | Of Position
    | Nil Position

    | Comma Position
    | Colon Position
    | Semicolon Position
    | LeftParen Position
    | RightParen Position
    | BeginSubscript Position
    | EndSubscript Position
    | BeginRecord Position
    | EndRecord Position
    | Member Position
    | Minus Position
    | Plus Position
    | Times Position
    | Equals Position
    | Assign Position

    | Int Integer Position
    | String String Position
    | Identifier String Position

    deriving (Eq, Show)