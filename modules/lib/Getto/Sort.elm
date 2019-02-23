module Getto.Sort exposing
  ( Model
  , SortOrder(..)
  , State
  , by
  , state
  , current
  , next
  , expose
  , fromString
  )

type Model = Model
  { column : String
  , order  : SortOrder
  }
type SortOrder
  = Up
  | Down

type State = State
  { current : Maybe SortOrder
  , next    : Model
  }

by : String -> Model
by col = Model
  { column = col
  , order  = Down
  }

state : Model -> String -> State
state (Model model) col =
  if col /= model.column
    then
      State
        { current = Nothing
        , next = Model
          { column = col
          , order = Down
          }
        }
    else
      State
        { current = model.order |> Just
        , next = Model
          { column = col
          , order =
            case model.order of
              Up   -> Down
              Down -> Up
          }
        }

current : State -> Maybe SortOrder
current (State model) = model.current

next : State -> Model
next (State model) = model.next


expose : Model -> ( String, String )
expose (Model model) =
  ( model.column
  , case model.order of
    Up   -> "up"
    Down -> "down"
  )

fromString : ( String, String ) -> Model
fromString (col,orderString) = Model
  { column = col
  , order  =
    case orderString |> String.toLower of
      "up" -> Up
      _    -> Down
  }
