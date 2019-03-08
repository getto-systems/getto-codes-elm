port module GettoUpload.Command.Dom exposing
  ( fill
  )

import Json.Encode as Encode

port fillFieldValues : Encode.Value -> Cmd msg

fill : List ( String, String ) -> Cmd annonymous
fill =
  List.map
    (\(id,value) ->
      [ ( "id",    id    |> Encode.string )
      , ( "value", value |> Encode.string )
      ]
    )
  >> Encode.list Encode.object
  >> fillFieldValues
