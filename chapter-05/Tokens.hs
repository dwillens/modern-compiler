module Tokens where
  import Position

  data Token =
      EOF
    | While Position
    | For Position
    | To Position
    | Break Position
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
    | Do Position
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
    | Divide Position
    | Equals Position
    | NotEquals Position
    | Less Position
    | LessOrEquals Position
    | Greater Position
    | GreaterOrEquals Position
    | And Position
    | Or Position
    | Assign Position

    | Int (Integer, Position)
    | String (String, Position)
    | Identifier (String, Position)

    deriving (Eq, Show)