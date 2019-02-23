module GettoUpload.App.Upload.Edit.Page exposing ( main )
import GettoUpload.App.Upload.Edit.Info as Info
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
  { info : Info.Model
  }

type alias FrameMsg = Frame.Msg Layout.Msg Msg
type Msg
  = Info Info.Msg

setup : Frame.SetupApp Layout.Model Model Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "info", model.info |> Info.store )
      ]
    , \value model ->
      Model
        ( model.info |> Info.storeChanged (value |> SafeDecode.valueAt ["info"]) )
    )
  , search =
    ( \model -> QueryEncode.object
      [ ( "info", model.info |> Info.query )
      ]
    , \value model ->
      Model
        ( model.info |> Info.queryChanged ["info"] value )
    )
  , dom = \model -> List.concat
    [ model.info |> Info.fill
    ]
  , init = init
  }

init : Frame.InitModel -> ( Model, FrameTransition )
init model =
  Transition.compose Model
    (model |> Info.init "info" |> Transition.map Info)

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.info |> Info.subscriptions |> Sub.map Info
  ] |> Sub.batch

info_ = Transition.prop .info (\v m -> { m | info = v })

update : Msg -> Model -> ( Model, FrameTransition )
update message =
  case message of
    Info msg ->
      Transition.update info_
        (Info.update msg >> Transition.map Info)

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
      ( model |> Info.contents |> Frame.mapApp Info ) ++
      [ model |> Layout.articleFooter ]
    , H.nav []
      [ model |> Layout.navHeader
      , model |> Layout.navAddress
      , model |> Layout.nav
      , model |> Layout.navFooter
      ]
    ] ++
    ( model |> Info.dialogs |> Frame.mapApp Info )
