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
import GettoUpload.Layout.Page.Article.Html as Html
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Static as Static
import GettoUpload.I18n.App as AppI18n

import Getto.Command.Transition as T exposing ( Transition )

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Lazy as L

type alias FrameModel a app = Frame.Model { a | article : Model } app
type alias FrameTransition a app = Transition (FrameModel a app) Msg
type alias Model = ()

type Msg
  = Noop

signature = "layout-article"

init : Frame.InitModel -> ( Model, FrameTransition a app )
init model =
  ( ()
  , T.none
  )

store : Model -> Encode.Value
store model = Encode.null

storeChanged : Decode.Value -> Model -> Model
storeChanged value model = model

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, FrameTransition a app )
update msg model =
  case msg of
    Noop -> ( model, T.none )


documentTitle : FrameModel a app -> String
documentTitle model =
  let
    static  = model |> Frame.static
    project = static |> Static.project
  in
    Html.documentTitle
      { path    = static |> Static.page |> .path
      , company = project.company
      , title   = project.title
      , i18n    = AppI18n.title
      }

header : FrameModel a app -> Html Msg
header model = L.lazy
  (\static -> Html.header
    { path = static |> Static.page |> .path
    , i18n = AppI18n.title
    }
  )
  (model |> Frame.static)

footer : FrameModel a app -> Html Msg
footer model = L.lazy
  (\static -> Html.footer
    { copyright = static |> Static.version |> .copyright
    }
  )
  (model |> Frame.static)
