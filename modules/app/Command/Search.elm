module GettoUpload.Command.Search exposing
  ( Model
  , Init
  , init
  , decode
  , pushUrl
  , loadUrl
  )
import GettoUpload.Extension.Href as Href exposing ( Href )

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Browser.Navigation as Navigation
import Url exposing ( Url )

type Model model = Model (Inner model)
type alias Inner model =
  { key    : Navigation.Key
  , encode : Encode model
  , decode : Decode model
  }

type alias Encode model = model -> QueryEncode.Value
type alias Decode model = QueryDecode.Value -> model -> model

type alias Init model = ( Encode model, Decode model )

init : Navigation.Key -> Init model -> Model model
init key (encoder,decoder) = Model
  { key    = key
  , encode = encoder
  , decode = decoder
  }

decode : Model model -> Url -> model -> model
decode (Model model) = split >> model.decode

split : Url -> QueryDecode.Value
split url =
  url.query
  |> Maybe.map QueryDecode.split
  |> Maybe.withDefault []

pushUrl : Model model -> model -> Cmd annonymous
pushUrl (Model model) = model.encode >> QueryEncode.encode >> Navigation.pushUrl model.key

loadUrl : Href -> Cmd annonymous
loadUrl = Href.toString >> Navigation.load
