module GettoCodes.App.Index.Dashboard.View exposing
  ( Example
  , example
  )

type alias Example =
  { current : String
  , target  : String
  , max     : String
  , percent : String
  }


type alias ExampleModel =
  { current : Int
  , target  : Int
  }

example : ExampleModel -> Example
example model =
  let
    target = model.target |> toFloat
  in
    { current = model.current  |> String.fromInt
    , target  = model.target   |> String.fromInt
    , max     = target * 3 / 2 |> String.fromFloat
    , percent = (model.current * 100 |> toFloat) / target |> floor |> String.fromInt
    }
