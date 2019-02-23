module Getto.Url.Query.SafeDecode exposing
  ( Value
  , Decoder
  , string
  , int
  , entryAt
  , listAt
  , boolAt
  , split
  )
import Getto.Url.Query.Encode as Encode
import Getto.Url.Query.Decode as Decode

import Url

type alias Value = List String

type alias Decoder a = Value -> a
type alias ValueDecoder a = Maybe String -> a

string : String -> ValueDecoder String
string default = Maybe.andThen Decode.string >> Maybe.withDefault default

int : Int -> ValueDecoder Int
int default = Maybe.andThen Decode.int >> Maybe.withDefault default

entryAt : List String -> ValueDecoder a -> Decoder a
entryAt names decoder =
  Decode.filter names ""
  >> List.head
  >> decoder

listAt : List String -> ValueDecoder a -> Decoder (List a)
listAt names decoder =
  Decode.filter names "[]"
  >> List.map (Just >> decoder)

boolAt : List String -> Decoder Bool
boolAt = Decode.find

split : String -> Value
split = Decode.split
