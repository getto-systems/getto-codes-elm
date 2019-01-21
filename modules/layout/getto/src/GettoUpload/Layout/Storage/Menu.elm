module GettoUpload.Layout.Storage.Menu exposing
  ( Model
  , init
  , encode
  , decoder
  , toggle
  , isCollapsed
  )

import Set exposing ( Set )
import Json.Encode as Encode
import Json.Decode as Decode

type alias Model =
  { collapsed : Set String
  }

init : Model
init =
  { collapsed = Set.empty
  }

encode : Model -> Encode.Value
encode model =
  [ ( "collapsed", model.collapsed |> Encode.set Encode.string )
  ] |> Encode.object

decoder : Decode.Decoder Model
decoder = Decode.map Model
  (Decode.at ["collapsed"] (Decode.list Decode.string |> Decode.map Set.fromList))

toggle : String -> Model -> Model
toggle name model =
  let
    collapsed = model.collapsed
  in
    { model
    | collapsed =
      if collapsed |> Set.member name
        then collapsed |> Set.remove name
        else collapsed |> Set.insert name
    }

isCollapsed : String -> Model -> Bool
isCollapsed name model = model.collapsed |> Set.member name
