module Getto.Json.SafeDecode exposing
  ( string
  , int
  , bool
  , list
  , at
  , valueAt
  )

import Json.Encode as Encode
import Json.Decode as Decode

type Decoder a
  = Decoder (Decode.Decoder a) a

string : String -> Decoder String
string default = Decoder (Decode.string |> withDefault default) default

int : Int -> Decoder Int
int default = Decoder (Decode.int |> withDefault default) default

bool : Bool -> Decoder Bool
bool default = Decoder (Decode.bool |> withDefault default) default

list : Decoder a -> Decoder (List a)
list decoder =
  case decoder of
    Decoder decode default ->
      Decoder (Decode.list decode |> withDefault []) []

withDefault : a -> Decode.Decoder a -> Decode.Decoder a
withDefault default decoder =
  Decode.oneOf
    [ decoder
    , Decode.succeed default
    ]

at : List String -> Decoder a -> Decode.Value -> a
at names decoder =
  case decoder of
    Decoder decode default ->
      Decode.decodeValue (Decode.at names decode)
      >> Result.withDefault default

valueAt : List String -> Decode.Value -> Decode.Value
valueAt names =
  Decode.decodeValue (Decode.at names Decode.value)
  >> Result.withDefault Encode.null
