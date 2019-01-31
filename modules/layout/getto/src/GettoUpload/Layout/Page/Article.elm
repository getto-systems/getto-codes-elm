module GettoUpload.Layout.Page.Article exposing
  ( Model
  , Msg
  , init
  , store
  , storeChanged
  , subscriptions
  , update
  , documentTitle
  , header
  , footer
  )
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Static as Static
import GettoUpload.I18n.App as I18n

import Getto.Command.Transition as Transition exposing ( Transition )

import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a app appMsg = Frame.Model { a | article : Model } app appMsg
type alias FrameTransition a app appMsg = Transition (FrameModel a app appMsg) Msg
type alias Model = {}

type Msg
  = Noop

init : Frame.InitModel -> ( Model, FrameTransition a app appMsg )
init model =
  ( {}
  , Transition.none
  )

store : Model -> Encode.Value
store model = Encode.null

storeChanged : Decode.Value -> Model -> Model
storeChanged value model = model

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, FrameTransition a app appMsg )
update msg model =
  case msg of
    Noop -> ( model, Transition.none )


title : Static.Model -> String
title = Static.page >> .path >> I18n.title

documentTitle : FrameModel a app appMsg -> String
documentTitle model =
  let
    static = model |> Frame.static
  in
    (static |> title)
    ++ " | "
    ++ (static |> Static.project |> .company)
    ++ " "
    ++ (static |> Static.project |> .title)

header : FrameModel a app appMsg -> Html Msg
header model =
  let
    static = model |> Frame.static
  in
    H.h1 [] [ static |> title |> H.text ]

footer : FrameModel a app appMsg -> Html Msg
footer model =
  let
    static = model |> Frame.static
  in
    H.footer []
      [ H.p []
        [ H.i [ A.class "far fa-copyright" ] []
        , " " |> H.text
        , static |> Static.version |> .copyright |> H.text
        ]
      ]
