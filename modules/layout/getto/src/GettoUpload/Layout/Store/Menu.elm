module GettoUpload.Layout.Store.Menu exposing
  ( Model
  , encode
  , decode
  , toggle
  , isCollapsed
  )

import Getto.Json.SafeDecode as SafeDecode

import Set exposing ( Set )
import Json.Encode as Encode
import Json.Decode as Decode

type alias Model =
  { collapsed : Set String
  }

encode : Model -> Encode.Value
encode model =
  [ ( "collapsed", model.collapsed |> Encode.set Encode.string )
  ] |> Encode.object

decode : Decode.Value -> Model
decode value =
  { collapsed = value |> SafeDecode.at ["collapsed"] (SafeDecode.list (SafeDecode.string "")) |> Set.fromList
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
