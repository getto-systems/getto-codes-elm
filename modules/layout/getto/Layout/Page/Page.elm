module GettoCodes.Layout.Page.Page exposing
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
import GettoCodes.Layout.Page.Model as Model
import GettoCodes.Layout.Page.Article as Article
import GettoCodes.Layout.Page.Side    as Side
import GettoCodes.Layout.Page.Options as Options
import GettoCodes.Layout.Frame as Frame

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode

import Html as H exposing ( Html )
import Html.Lazy as L

type alias Model = Model.Page

type Msg
  = Article Article.Msg
  | Side Side.Msg
  | Options Options.Msg

setup : Frame.SetupLayout Model.Page app Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "article", model.article |> Article.encodeStore )
      , ( "side",    model.side    |> Side.encodeStore )
      , ( "options", model.options |> Options.encodeStore )
      ]
    , \value model ->
      Model.Page
        ( model.article |> Article.decodeStore (value |> SafeDecode.valueAt ["article"]) )
        ( model.side    |> Side.decodeStore    (value |> SafeDecode.valueAt ["side"]) )
        ( model.options |> Options.decodeStore (value |> SafeDecode.valueAt ["options"]) )
    )
  , init = init
  }

init : Frame.InitModel -> ( Model.Page, Model.Transition app Msg )
init model =
  T.compose3 Model.Page
    (model |> Article.init |> T.map Article)
    (model |> Side.init    |> T.map Side)
    (model |> Options.init |> T.map Options)

subscriptions : Model.Page -> Sub Msg
subscriptions model =
  [ model.article |> Article.subscriptions |> Sub.map Article
  , model.side    |> Side.subscriptions    |> Sub.map Side
  , model.options |> Options.subscriptions |> Sub.map Options
  ] |> Sub.batch

article_ = T.prop .article (\v m -> { m | article = v })
side_    = T.prop .side    (\v m -> { m | side = v })
options_ = T.prop .options (\v m -> { m | options = v })

update : Msg -> Model.Page -> ( Model.Page, Model.Transition app Msg )
update message =
  case message of
    Article msg -> T.update article_ (Article.update msg >> T.map Article)
    Side    msg -> T.update side_    (Side.update msg    >> T.map Side)
    Options msg -> T.update options_ (Options.update msg >> T.map Options)


documentTitle : Model.Frame app -> String
documentTitle = Article.documentTitle

articleHeader : Model.Frame app -> Html (Frame.Msg Msg appMsg)
articleHeader = Article.header >> H.map Article >> Frame.mapLayout

articleFooter : Model.Frame app -> Html (Frame.Msg Msg appMsg)
articleFooter = Article.footer >> H.map Article >> Frame.mapLayout

mobileHeader : Model.Frame app -> Html (Frame.Msg Msg appMsg)
mobileHeader = Side.mobileHeader >> H.map Side >> Frame.mapLayout

mobileAddress : Model.Frame app -> Html (Frame.Msg Msg appMsg)
mobileAddress = Side.navAddress >> H.map Side >> Frame.mapLayout

breadcrumb : Model.Frame app -> Html (Frame.Msg Msg appMsg)
breadcrumb = Side.breadcrumb >> H.map Side >> Frame.mapLayout

navHeader : Model.Frame app -> Html (Frame.Msg Msg appMsg)
navHeader = Side.navHeader >> H.map Side >> Frame.mapLayout

navAddress : Model.Frame app -> Html (Frame.Msg Msg appMsg)
navAddress = Side.navAddress >> H.map Side >> Frame.mapLayout

nav : Model.Frame app -> Html (Frame.Msg Msg appMsg)
nav = Side.nav >> H.map Side >> Frame.mapLayout

navFooter : Model.Frame app -> Html (Frame.Msg Msg appMsg)
navFooter = Side.navFooter >> H.map Side >> Frame.mapLayout
