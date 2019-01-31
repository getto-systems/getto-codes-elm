module GettoUpload.App.Index.Dashboard.View exposing
  ( Example
  , example
  )

type alias Example =
  { current : String
  , target  : String
  , max     : String
  , percent : String
  }


example : { current : Int, target : Int } -> Example
example model =
  let
    all = (model.target * 3 |> toFloat) / 2
  in
    { current = model.current |> String.fromInt
    , target  = model.target  |> String.fromInt
    , max     = all |> String.fromFloat
    , percent = (model.current * 100 |> toFloat) / all |> floor |> String.fromInt
    }
