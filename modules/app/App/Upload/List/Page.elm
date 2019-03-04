module GettoUpload.App.Upload.List.Page exposing ( main )
import GettoUpload.App.Upload.List.Search as Search
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Page.Page as Layout

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode

import Browser
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

main = Browser.application
  { init          = Frame.init Layout.setup setup
  , subscriptions = Frame.subscriptions Layout.subscriptions subscriptions
  , onUrlRequest  = Frame.onUrlRequest
  , onUrlChange   = Frame.onUrlChange
  , update        = Frame.update Layout.update update
  , view          = document
  }

type alias FrameModel = Frame.Model Layout.Model Model
type alias FrameTransition = Transition FrameModel Msg
type alias Model =
  { search : Search.Model
  }

type alias FrameMsg = Frame.Msg Layout.Msg Msg
type Msg
  = Search Search.Msg

setup : Frame.SetupApp Layout.Model Model Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "search", model.search |> Search.encodeStore )
      ]
    , \value model -> Model
      ( model.search |> Search.decodeStore (value |> SafeDecode.valueAt ["search"]) )
    )
  , search =
    ( \model -> model.search |> Search.encodeQuery
    , \value model -> { model | search = model.search |> Search.decodeQuery [] value }
    )
  , init = init
  }

init : Frame.InitModel -> ( Model, FrameTransition )
init model =
  T.compose Model
    (model |> Search.init |> T.map Search)

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.search |> Search.subscriptions |> Sub.map Search
  ] |> Sub.batch

search_ = T.prop .search (\v m -> { m | search = v })

update : Msg -> Model -> ( Model, FrameTransition )
update message =
  case message of
    Search msg -> T.update search_ (Search.update msg >> T.map Search)

document : FrameModel -> Browser.Document FrameMsg
document model =
  { title = model |> Layout.documentTitle
  , body = [ L.lazy content model ]
  }

content : FrameModel -> Html FrameMsg
content model =
  H.section [ A.class "MainLayout" ] <| List.concat
    [ [ model |> Layout.mobileHeader
      , model |> Layout.mobileAddress
      , H.article [] <| List.concat
        [ [ H.header []
            [ model |> Layout.articleHeader
            , model |> Layout.breadcrumb
            ]
          ]
        , model |> Search.contents |> Frame.mapApp Search
        , [ model |> Layout.articleFooter ]
        ]
      , H.nav []
        [ model |> Layout.navHeader
        , model |> Layout.navAddress
        , model |> Layout.nav
        , model |> Layout.navFooter
        ]
      ]
    , model |> Search.dialogs |> Frame.mapApp Search
    ]
