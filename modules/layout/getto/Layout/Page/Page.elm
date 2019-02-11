module GettoUpload.Layout.Page.Page exposing
  ( Model
  , Msg
  , setup
  , subscriptions
  , update
  , documentTitle
  , articleHeader
  , articleFooter
  , mobileHeader
  , mobileAddress
  , breadcrumb
  , navHeader
  , navAddress
  , nav
  , navFooter
  )
import GettoUpload.Layout.Page.Side    as Side
import GettoUpload.Layout.Page.Article as Article
import GettoUpload.Layout.Frame as Frame

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Lazy as L

type alias FrameModel app = Frame.Model Model app
type alias FrameTransition app = Transition (FrameModel app) Msg
type alias Model =
  { article : Article.Model
  , side    : Side.Model
  }

type alias FrameMsg appMsg = Frame.Msg Msg appMsg
type Msg
  = Article Article.Msg
  | Side    Side.Msg

setup : Frame.SetupLayout Model app Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "article", model.article |> Article.store )
      , ( "side",    model.side    |> Side.store )
      ]
    , \value model ->
      Model
        ( model.article |> Article.storeChanged (value |> SafeDecode.valueAt ["article"]) )
        ( model.side    |> Side.storeChanged    (value |> SafeDecode.valueAt ["side"]) )
    )
  , init  = init
  }

init : Frame.InitModel -> ( Model, FrameTransition app )
init model =
  Transition.compose2 Model
    (model |> Article.init |> Transition.map Article)
    (model |> Side.init    |> Transition.map Side)

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.article |> Article.subscriptions |> Sub.map Article
  , model.side    |> Side.subscriptions    |> Sub.map Side
  ] |> Sub.batch

update : Msg -> Model -> ( Model, FrameTransition app )
update message =
  case message of
    Article msg ->
      Transition.update
        .article (\article m -> { m | article = article })
        (Article.update msg >> Transition.map Article)

    Side msg ->
      Transition.update
        .side (\side m -> { m | side = side })
        (Side.update msg >> Transition.map Side)


documentTitle : FrameModel app -> String
documentTitle = Article.documentTitle

articleHeader : FrameModel app -> Html (FrameMsg appMsg)
articleHeader = Article.header >> H.map Article >> Frame.mapLayout

articleFooter : FrameModel app -> Html (FrameMsg appMsg)
articleFooter = Article.footer >> H.map Article >> Frame.mapLayout

mobileHeader : FrameModel app -> Html (FrameMsg appMsg)
mobileHeader = Side.mobileHeader >> H.map Side >> Frame.mapLayout

mobileAddress : FrameModel app -> Html (FrameMsg appMsg)
mobileAddress = Side.navAddress >> H.map Side >> Frame.mapLayout

breadcrumb : FrameModel app -> Html (FrameMsg appMsg)
breadcrumb = Side.breadcrumb >> H.map Side >> Frame.mapLayout

navHeader : FrameModel app -> Html (FrameMsg appMsg)
navHeader = Side.navHeader >> H.map Side >> Frame.mapLayout

navAddress : FrameModel app -> Html (FrameMsg appMsg)
navAddress = Side.navAddress >> H.map Side >> Frame.mapLayout

nav : FrameModel app -> Html (FrameMsg appMsg)
nav = Side.nav >> H.map Side >> Frame.mapLayout

navFooter : FrameModel app -> Html (FrameMsg appMsg)
navFooter = Side.navFooter >> H.map Side >> Frame.mapLayout
