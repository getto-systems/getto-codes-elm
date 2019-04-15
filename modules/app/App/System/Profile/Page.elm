module GettoCodes.App.System.Profile.Page exposing ( main )
import GettoCodes.App.System.Profile.Model as Model
import GettoCodes.App.System.Profile.Dashboard as Dashboard
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Page.Page as Layout

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

type Msg
  = Dashboard Dashboard.Msg

setup : Frame.SetupApp Layout.Model Model.Page Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "dashboard", model.dashboard |> Dashboard.encodeStore )
      ]
    , \value model -> Model.Page
      ( model.dashboard |> Dashboard.decodeStore (value |> SafeDecode.valueAt ["dashboard"]) )
    )
  , search =
    ( \model -> model.dashboard |> Dashboard.encodeQuery
    , \value model -> { model | dashboard = model.dashboard |> Dashboard.decodeQuery [] value }
    )
  , init = init
  }

init : Frame.InitModel -> ( Model.Page, Model.Transition Msg )
init model =
  T.compose Model.Page
    (model |> Dashboard.init |> T.map Dashboard)

subscriptions : Model.Page -> Sub Msg
subscriptions model =
  [ model.dashboard |> Dashboard.subscriptions |> Sub.map Dashboard
  ] |> Sub.batch

dashboard_ = T.prop .dashboard (\v m -> { m | dashboard = v })

update : Msg -> Model.Page -> ( Model.Page, Model.Transition Msg )
update message =
  case message of
    Dashboard msg -> T.update dashboard_ (Dashboard.update msg >> T.map Dashboard)

document : Model.Frame -> Browser.Document (Model.Msg Msg)
document model =
  { title = model |> Layout.documentTitle
  , body = [ L.lazy content model ]
  }

content : Model.Frame -> Html (Model.Msg Msg)
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
    ]
