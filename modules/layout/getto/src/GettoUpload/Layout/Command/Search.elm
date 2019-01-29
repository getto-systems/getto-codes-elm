module GettoUpload.Layout.Command.Search exposing
  ( Model
  , Init
  , init
  , changed
  , search
  )

import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Browser.Navigation as Navigation
import Url exposing ( Url )

type Model model msg = Model (Inner model msg)
type alias Inner model msg =
  { key    : Navigation.Key
  , encode : Encode model
  , decode : Decode model msg
  }

type alias Encode model = model -> QueryEncode.Value
type alias Decode model msg = QueryDecode.Value -> model -> ( model, Cmd msg )

type alias Init model msg = ( Encode model, Decode model msg )

init : Navigation.Key -> Init model msg -> Model model msg
init key (encode,decode) = Model
  { key    = key
  , encode = encode
  , decode = decode
  }

changed : Model model msg -> Url -> model -> ( model, Cmd msg )
changed (Model model) = split >> model.decode

split : Url -> QueryDecode.Value
split url =
  url.query
  |> Maybe.map QueryDecode.split
  |> Maybe.withDefault []

search : Model model msg -> model -> Cmd annonymous
search (Model model) = model.encode >> QueryEncode.encode >> Navigation.pushUrl model.key
