module GettoUpload.Layout.Page.Article exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , documentTitle
  , header
  , footer
  )
import GettoUpload.Layout.Page.Model as Model
import GettoUpload.Layout.Page.Article.Html as Html
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Static as Static
import GettoUpload.I18n.App as AppI18n

import Getto.Command.Transition as T exposing ( Transition )

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Lazy as L

type Msg
  = Noop

signature = "layout-article"

init : Frame.InitModel -> ( Model.Article, Model.Transition app Msg )
init model =
  ( ()
  , T.none
  )

encodeStore : Model.Article -> Encode.Value
encodeStore model = Encode.null

decodeStore : Decode.Value -> Model.Article -> Model.Article
decodeStore value model = model

subscriptions : Model.Article -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model.Article -> ( Model.Article, Model.Transition app Msg )
update msg model =
  case msg of
    Noop -> ( model, T.none )


documentTitle : Model.Frame app -> String
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

header : Model.Frame app -> Html Msg
header model = L.lazy
  (\static -> Html.header
    { path = static |> Static.page |> .path
    , i18n = AppI18n.title
    }
  )
  (model |> Frame.static)

footer : Model.Frame app -> Html Msg
footer model = L.lazy
  (\static -> Html.footer
    { copyright = static |> Static.version |> .copyright
    }
  )
  (model |> Frame.static)
