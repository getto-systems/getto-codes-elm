module GettoUpload.App.Upload.New.Page exposing ( main )
import GettoUpload.App.Upload.New.Model as Model
import GettoUpload.App.Upload.New.Register as Register
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

type Msg
  = Register Register.Msg

setup : Frame.SetupApp Layout.Model Model.Page Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "register", model.register |> Register.encodeStore )
      ]
    , \value model -> Model.Page
      ( model.register |> Register.decodeStore (value |> SafeDecode.valueAt ["register"]) )
    )
  , search =
    ( \model -> model.register |> Register.encodeQuery
    , \value model -> { model | register = model.register |> Register.decodeQuery [] value }
    )
  , init = init
  }

init : Frame.InitModel -> ( Model.Page, Model.Transition Msg )
init model =
  T.compose Model.Page
    (model |> Register.init |> T.map Register)

subscriptions : Model.Page -> Sub Msg
subscriptions model =
  [ model.register |> Register.subscriptions |> Sub.map Register
  ] |> Sub.batch

register_ = T.prop .register (\v m -> { m | register = v })

update : Msg -> Model.Page -> ( Model.Page, Model.Transition Msg )
update message =
  case message of
    Register msg -> T.update register_ (Register.update msg >> T.map Register)

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
        , model |> Register.contents |> Frame.mapApp Register
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
