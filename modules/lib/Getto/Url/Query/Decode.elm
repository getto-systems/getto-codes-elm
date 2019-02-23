module Getto.Url.Query.Decode exposing
  ( Value
  , Decoder
  , string
  , int
  , entryAt
  , listAt
  , filter
  , boolAt
  , find
  , split
  )
import Getto.Url.Query.Encode as Encode

import Url

type alias Value = List String

type alias Decoder a = Value -> Maybe a
type alias ValueDecoder a = String -> Maybe a

string : ValueDecoder String
string = Url.percentDecode

int : ValueDecoder Int
int = String.toInt

entryAt : List String -> ValueDecoder a -> Decoder a
entryAt names decoder =
  filter names ""
  >> List.head
  >> Maybe.andThen decoder

listAt : List String -> ValueDecoder a -> Decoder (List a)
listAt names decoder list =
  case list |> filter names "[]" of
    [] -> Nothing
    values ->
      let
        decoded = values |> List.map decoder
      in
        if decoded |> List.any ((==) Nothing)
          then Nothing
          else decoded |> List.filterMap identity |> Just

filter : List String -> String -> List String -> List String
filter names suffix =
  let
    name = ((names |> Encode.toName) ++ suffix |> Url.percentEncode) ++ "="
    length = name |> String.length
  in
    List.filterMap
      (\query ->
        if query |> String.startsWith name
          then query |> String.dropLeft length |> Just
          else Nothing
      )

boolAt : List String -> Decoder Bool
boolAt names = find names >> Just

find : List String -> List String -> Bool
find names = List.any ((==) (names |> Encode.toName |> Url.percentEncode))

split : String -> Value
split = String.split "&"
