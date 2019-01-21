port module GettoUpload.Layout.Command.Search exposing
  ( Model
  , init
  , pushUrl
  , exec
  )

import Getto.Url.Query.Encode as QueryEncode

import Browser.Navigation as Navigation

type Model
  = None
  | PushUrl QueryEncode.Value

init : Model
init = None

pushUrl : QueryEncode.Value -> Model -> Model
pushUrl q model = PushUrl q

exec : Navigation.Key -> Model -> ( Model, Cmd msg )
exec key model =
  ( None
  , case model of
    None -> Cmd.none
    PushUrl q -> q |> QueryEncode.encode |> Navigation.pushUrl key
  )
