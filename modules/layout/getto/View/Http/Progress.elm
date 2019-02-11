module GettoUpload.View.Http.Progress exposing
  ( Model
  , percentage
  )

type alias Model =
  { current : Int
  , size    : Int
  }

percentage : Model -> Int
percentage model = model.current * 100 // model.size
