module GettoUpload.App.Upload.New.Page exposing ( main )
import GettoUpload.App.Upload.New.Register as Register
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Page.Page as Layout

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode
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
  { register : Register.Model
  }

type alias FrameMsg = Frame.Msg Layout.Msg Msg
type Msg
  = Register Register.Msg

setup : Frame.SetupApp Layout.Model Model Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "register", model.register |> Register.store )
      ]
    , \value model ->
      Model
        ( model.register |> Register.storeChanged (value |> SafeDecode.valueAt ["register"]) )
    )
  , search =
    ( \model -> QueryEncode.object
      [ ( "register", model.register |> Register.query )
      ]
    , \value model ->
      Model
        ( model.register |> Register.queryChanged ["register"] value )
    )
  , dom = \model -> List.concat
    [ model.register |> Register.fill
    ]
  , init = init
  }

init : Frame.InitModel -> ( Model, FrameTransition )
init model =
  Transition.compose Model
    (model |> Register.init "register" |> Transition.map Register)

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.register |> Register.subscriptions |> Sub.map Register
  ] |> Sub.batch

register_ = Transition.prop .register (\v m -> { m | register = v })

update : Msg -> Model -> ( Model, FrameTransition )
update message =
  case message of
    Register msg ->
      Transition.update register_
        (Register.update msg >> Transition.map Register)

document : FrameModel -> Browser.Document FrameMsg
document model =
  { title = model |> Layout.documentTitle
  , body = [ L.lazy content model ]
  }

content : FrameModel -> Html FrameMsg
content model =
  H.section [ A.class "MainLayout" ] <|
    [ model |> Layout.mobileHeader
    , model |> Layout.mobileAddress
    , H.article [] <|
      [ H.header []
        [ model |> Layout.articleHeader
        , model |> Layout.breadcrumb
        ]
      ] ++
      ( model |> Register.contents |> Frame.mapApp Register ) ++
      [ model |> Layout.articleFooter ]
    , H.nav []
      [ model |> Layout.navHeader
      , model |> Layout.navAddress
      , model |> Layout.nav
      , model |> Layout.navFooter
      ]
    ] ++
    ( model |> Register.dialogs |> Frame.mapApp Register )
