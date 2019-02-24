port module GettoUpload.Command.Dom exposing
  ( fill
  , string
  )

import Getto.Field as Field

import Json.Encode as Encode

port fillFieldValues : Encode.Value -> Cmd msg

type alias Value = ( String, String )

fill : List Value -> Cmd annonymous
fill =
  List.map
    (\(id,value) ->
      [ ( "id",    id    |> Encode.string )
      , ( "value", value |> Encode.string )
      ]
    )
  >> Encode.list Encode.object
  >> fillFieldValues

string : Field.Model String -> Value
string field =
  ( field |> Field.id
  , field |> Field.value
  )
