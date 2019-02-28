module Getto.Http.Header.Decode exposing
  ( Value
  , Error
  , Decoder
  , string
  , int
  , maybe
  , at
  , succeed
  , fail
  , decode
  , map
  , map2
  , map3
  , map4
  , map5
  , errorToString
  )

import Dict exposing ( Dict )

type alias Value = Dict String String

type alias Decoder a = Value -> Result DecodeError a
type alias EntryDecoder a = Maybe String -> Result DecodeError a

type Error = Error Value DecodeError

type DecodeError
  = Failure String
  | At String DecodeError

string : EntryDecoder String
string = Result.fromMaybe (Failure "not exists")

int : EntryDecoder Int
int = string >> Result.andThen
  (\value ->
    case value |> String.toInt of
      Nothing  -> Failure ("parse failed: " ++ value) |> Err
      Just val -> Ok val
  )

maybe : EntryDecoder a -> EntryDecoder (Maybe a)
maybe decoder entry =
  case entry of
    Nothing -> Ok Nothing
    Just _ -> entry |> decoder |> Result.map Just

at : String -> EntryDecoder a -> Decoder a
at key decoder = Dict.get key >> decoder >> Result.mapError (At key)

succeed : a -> Decoder a
succeed value = always <| Ok value

fail : String -> Decoder a
fail error = always <| Err (Failure error)

decode : Decoder a -> Value -> Result Error a
decode decoder value = value |> decoder |> Result.mapError (Error value)

map : (a -> b) -> Decoder a -> Decoder b
map f decoderA headers =
  Result.map f
    (headers |> decoderA)

map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f decoderA decoderB headers =
  Result.map2 f
    (headers |> decoderA)
    (headers |> decoderB)

map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f decoderA decoderB decoderC headers =
  Result.map3 f
    (headers |> decoderA)
    (headers |> decoderB)
    (headers |> decoderC)

map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 f decoderA decoderB decoderC decoderD headers =
  Result.map4 f
    (headers |> decoderA)
    (headers |> decoderB)
    (headers |> decoderC)
    (headers |> decoderD)

map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 f decoderA decoderB decoderC decoderD decoderE headers =
  Result.map5 f
    (headers |> decoderA)
    (headers |> decoderB)
    (headers |> decoderC)
    (headers |> decoderD)
    (headers |> decoderE)

errorToString : Error -> String
errorToString (Error value error) = ( error |> decodeErrorToString ) ++ ": " ++ ( value |> valueToString )

decodeErrorToString : DecodeError -> String
decodeErrorToString error =
  case error of
    Failure err -> err
    At  key err -> key ++ " " ++ (err |> decodeErrorToString)

valueToString : Value -> String
valueToString = Dict.toList >> List.map (\(k,v) -> k ++ ":" ++ v) >> String.join ","
