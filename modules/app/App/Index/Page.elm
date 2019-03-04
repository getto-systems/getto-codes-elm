module GettoUpload.App.Index.Page exposing ( main )
import GettoUpload.App.Index.Dashboard as Dashboard
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
  { dashboard : Dashboard.Model
  }

type alias FrameMsg = Frame.Msg Layout.Msg Msg
type Msg
  = Dashboard Dashboard.Msg

setup : Frame.SetupApp Layout.Model Model Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "dashboard", model.dashboard |> Dashboard.encodeStore )
      ]
    , \value model -> Model
      ( model.dashboard |> Dashboard.decodeStore (value |> SafeDecode.valueAt ["dashboard"]) )
    )
  , search =
    ( \model -> model.dashboard |> Dashboard.encodeQuery
    , \value model -> { model | dashboard = model.dashboard |> Dashboard.decodeQuery [] value }
    )
  , init = init
  }

init : Frame.InitModel -> ( Model, FrameTransition )
init model =
  T.compose Model
    (model |> Dashboard.init |> T.map Dashboard)

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.dashboard |> Dashboard.subscriptions |> Sub.map Dashboard
  ] |> Sub.batch

dashboard_ = T.prop .dashboard (\v m -> { m | dashboard = v })

update : Msg -> Model -> ( Model, FrameTransition )
update message =
  case message of
    Dashboard msg -> T.update dashboard_ (Dashboard.update msg >> T.map Dashboard)

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
        , model |> Dashboard.contents |> Frame.mapApp Dashboard
        , [ model |> Layout.articleFooter ]
        ]
      , H.nav []
        [ model |> Layout.navHeader
        , model |> Layout.navAddress
        , model |> Layout.nav
        , model |> Layout.navFooter
        ]
      ]
    , model |> Dashboard.dialogs |> Frame.mapApp Dashboard
    ]
