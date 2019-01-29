module Getto.Http.Part exposing
  ( Model
  , string
  , file
  , bytes
  , list
  , object
  , toBody
  )

import Getto.Url.Query.Encode as QueryEncode

import File exposing ( File )
import Bytes exposing ( Bytes )
import Http

type Model
  = StringPart String
  | FilePart File
  | BytesPart String Bytes
  | ListPart (List Model)
  | ObjectPart (List ( String, Model ))

type Entry
  = StringEntry String
  | FileEntry File
  | BytesEntry String Bytes

string : String -> Model
string = StringPart

file : File -> Model
file = FilePart

bytes : String -> Bytes -> Model
bytes = BytesPart

list : List Model -> Model
list = ListPart

object : List ( String, Model ) -> Model
object = ObjectPart

toBody : Model -> Http.Body
toBody =
  flatten []
  >> List.map toPart
  >> Http.multipartBody

flatten : List String -> Model -> List ( List String, Entry )
flatten current part =
  case part of
    StringPart            value -> [ ( current, value |> StringEntry ) ]
    FilePart              value -> [ ( current, value |> FileEntry ) ]
    BytesPart contentType value -> [ ( current, value |> BytesEntry contentType ) ]
    ListPart   parts -> parts |> List.concatMap (flatten (current ++ [""]))
    ObjectPart parts -> parts |> List.concatMap (\(name,p) -> p |> flatten (current ++ [name]))

toPart : ( List String, Entry ) -> Http.Part
toPart (names,part) =
  let
    name = names |> QueryEncode.toName
  in
    case part of
      StringEntry            value -> value |> Http.stringPart name
      FileEntry              value -> value |> Http.filePart   name
      BytesEntry contentType value -> value |> Http.bytesPart  name contentType
