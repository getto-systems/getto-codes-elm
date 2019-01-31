module GettoUpload.Layout.Command.Search exposing
  ( Model
  , Init
  , init
  , changed
  , search
  )

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Browser.Navigation as Navigation
import Url exposing ( Url )

type Model model m msg = Model (Inner model m msg)
type alias Inner model m msg =
  { key    : Navigation.Key
  , encode : Encode model
  , decode : Decode model m msg
  }

type alias Encode model = model -> QueryEncode.Value
type alias Decode model m msg = QueryDecode.Value -> model -> ( model, Transition m msg )

type alias Init model m msg = ( Encode model, Decode model m msg )

init : Navigation.Key -> Init model m msg -> Model model m msg
init key (encode,decode) = Model
  { key    = key
  , encode = encode
  , decode = decode
  }

changed : Model model m msg -> Url -> model -> ( model, Transition m msg )
changed (Model model) = split >> model.decode

split : Url -> QueryDecode.Value
split url =
  url.query
  |> Maybe.map QueryDecode.split
  |> Maybe.withDefault []

search : Model model m msg -> model -> Cmd annonymous
search (Model model) = model.encode >> QueryEncode.encode >> Navigation.pushUrl model.key
