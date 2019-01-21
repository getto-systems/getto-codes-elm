module GettoUpload.Layout.Storage.Menu exposing
  ( Model
  , encode
  , decode
  , toggle
  , isCollapsed
  )

import Getto.Json.Decode as Decode

import Set exposing ( Set )
import Json.Encode as Encode

type alias Model =
  { collapsed : Set String
  }

encode : Model -> Encode.Value
encode model =
  [ ( "collapsed", model.collapsed |> Encode.set Encode.string )
  ] |> Encode.object

decode : Decode.Value -> Model
decode value =
  { collapsed = value |> Decode.at ["collapsed"] (Decode.list (Decode.string "")) |> Set.fromList
  }

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
