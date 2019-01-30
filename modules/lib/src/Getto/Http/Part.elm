module Getto.Http.Part exposing
  ( Value
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

type Value
  = StringPart String
  | FilePart File
  | BytesPart String Bytes
  | ListPart (List Value)
  | ObjectPart (List ( String, Value ))

type Entry
  = StringEntry String
  | FileEntry File
  | BytesEntry String Bytes

string : String -> Value
string = StringPart

file : File -> Value
file = FilePart

bytes : String -> Bytes -> Value
bytes = BytesPart

list : List Value -> Value
list = ListPart

object : List ( String, Value ) -> Value
object = ObjectPart

toBody : Value -> Http.Body
toBody =
  flatten []
  >> List.map toPart
  >> Http.multipartBody

flatten : List String -> Value -> List ( List String, Entry )
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
