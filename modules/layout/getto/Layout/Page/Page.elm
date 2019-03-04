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

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode

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
  T.compose2 Model
    (model |> Article.init |> T.map Article)
    (model |> Side.init    |> T.map Side)

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.article |> Article.subscriptions |> Sub.map Article
  , model.side    |> Side.subscriptions    |> Sub.map Side
  ] |> Sub.batch

article_ = T.prop .article (\v m -> { m | article = v })
side_    = T.prop .side    (\v m -> { m | side = v })

update : Msg -> Model -> ( Model, FrameTransition app )
update message =
  case message of
    Article msg -> T.update article_ (Article.update msg >> T.map Article)
    Side    msg -> T.update side_    (Side.update msg    >> T.map Side)


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
