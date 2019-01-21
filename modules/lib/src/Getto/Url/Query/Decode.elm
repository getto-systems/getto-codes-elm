module Getto.Url.Query.Decode exposing
  ( Decoder
  , string
  , int
  , entryAt
  , listAt
  , boolAt
  , split
  )
import Getto.Url.Query.Encode as Encode

import Url

type alias Decoder a = List String -> a
type alias ValueDecoder a = Maybe String -> a

string : String -> ValueDecoder String
string default = Maybe.andThen Url.percentDecode >> Maybe.withDefault default

int : Int -> ValueDecoder Int
int default = Maybe.andThen String.toInt >> Maybe.withDefault default

entryAt : List String -> ValueDecoder a -> Decoder a
entryAt names decoder =
  filter names "="
  >> List.head
  >> decoder

listAt : List String -> ValueDecoder a -> Decoder (List a)
listAt names decoder =
  filter names "[]="
  >> List.map (Just >> decoder)

boolAt : List String -> Decoder Bool
boolAt names = List.any ((==) (names |> Encode.toName))

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

split : Decoder a -> String -> a
split decoder = String.split "&" >> decoder
