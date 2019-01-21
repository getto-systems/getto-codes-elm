module Getto.Url.Query.Decode exposing
  ( string
  , int
  , entryAt
  , boolAt
  , listAt
  , split
  )
import Getto.Url.Query.Encode as Encode

import Url

type alias Decoder a = Maybe String -> a
type alias Picker a = List String -> a

string : String -> Decoder String
string default = Maybe.andThen Url.percentDecode >> Maybe.withDefault default

int : Int -> Decoder Int
int default = Maybe.andThen String.toInt >> Maybe.withDefault default

entryAt : List String -> Decoder a -> Picker a
entryAt names decoder =
  filter names "="
  >> List.head
  >> decoder

boolAt : List String -> Picker Bool
boolAt names = List.any ((==) (names |> Encode.toName))

listAt : List String -> Decoder a -> Picker (List a)
listAt names decoder =
  filter names "[]="
  >> List.map (Just >> decoder)

filter : List String -> String -> List String -> List String
filter names suffix =
  let
    name = (names |> List.map Url.percentEncode |> Encode.toName) ++ suffix
    length = name |> String.length
  in
    List.filterMap
      (\query ->
        if query |> String.startsWith name
          then query |> String.dropLeft length |> Just
          else Nothing
      )

split : Picker a -> String -> a
split decoder = String.split "&" >> decoder
