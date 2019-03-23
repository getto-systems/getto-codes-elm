module GettoCodes.App.Data.Upload.ListEdit.Page exposing ( main )
import GettoCodes.App.Data.Upload.ListEdit.Model as Model
import GettoCodes.App.Data.Upload.ListEdit.Search as Search
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
  = Search Search.Msg

setup : Frame.SetupApp Layout.Model Model.Page Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "search", model.search |> Search.encodeStore )
      ]
    , \value model -> Model.Page
      ( model.search |> Search.decodeStore (value |> SafeDecode.valueAt ["search"]) )
    )
  , search =
    ( \model -> model.search |> Search.encodeQuery
    , \value model -> { model | search = model.search |> Search.decodeQuery [] value }
    )
  , init = init
  }

init : Frame.InitModel -> ( Model.Page, Model.Transition Msg )
init model =
  T.compose Model.Page
    (model |> Search.init |> T.map Search)

subscriptions : Model.Page -> Sub Msg
subscriptions model =
  [ model.search |> Search.subscriptions |> Sub.map Search
  ] |> Sub.batch

search_ = T.prop .search (\v m -> { m | search = v })

update : Msg -> Model.Page -> ( Model.Page, Model.Transition Msg )
update message =
  case message of
    Search msg -> T.update search_ (Search.update msg >> T.map Search)

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
