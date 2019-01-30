module GettoUpload.Layout.Frame.Page exposing
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
  , navHeader
  , navAddress
  , navBody
  , navFooter
  )
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Menu    as Menu
import GettoUpload.Layout.Frame.Article as Article
import GettoUpload.Layout.Command.Store as Store

import Getto.Command.Transition as Transition
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel app appMsg = Frame.Model Model app appMsg
type alias Command    app appMsg = Transition.Command (FrameModel app appMsg) Msg
type alias Model =
  { article : Article.Model
  , menu    : Menu.Model
  }

type alias FrameMsg appMsg = Frame.Msg Msg appMsg
type Msg
  = Article Article.Msg
  | Menu    Menu.Msg

setup : Frame.SetupLayout Model app appMsg Msg
setup =
  { store = store
  , init  = init
  }

init : Frame.InitModel -> ( Model, Command app appMsg )
init model =
  Transition.compose2 Transition.batch Model
    (model |> Article.init |> Transition.mapCommand Article)
    (model |> Menu.init    |> Transition.mapCommand Menu)

store : Store.Init Model
store =
  ( \model ->
    [ ( "article", model.article |> Article.store )
    , ( "menu",    model.menu    |> Menu.store )
    ] |> Encode.object
  , \value model ->
    Model
      (model.article |> Article.storeChanged (value |> SafeDecode.valueAt ["article"]))
      (model.menu    |> Menu.storeChanged    (value |> SafeDecode.valueAt ["menu"]))
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.article |> Article.subscriptions |> Sub.map Article
  , model.menu    |> Menu.subscriptions    |> Sub.map Menu
  ] |> Sub.batch

update : Msg -> Model -> ( Model, Command app appMsg )
update message =
  case message of
    Article msg ->
      Transition.update
        .article (\article m -> { m | article = article })
        (Article.update msg >> Transition.mapCommand Article)

    Menu msg ->
      Transition.update
        .menu (\menu m -> { m | menu = menu })
        (Menu.update msg >> Transition.mapCommand Menu)


documentTitle : FrameModel app appMsg -> String
documentTitle = Article.documentTitle

articleHeader : FrameModel app appMsg -> Html (FrameMsg appMsg)
articleHeader = Article.header >> H.map Article >> Frame.mapLayout

articleFooter : FrameModel app appMsg -> Html (FrameMsg appMsg)
articleFooter = Article.footer >> H.map Article >> Frame.mapLayout

mobileHeader : FrameModel app appMsg -> Html (FrameMsg appMsg)
mobileHeader = Menu.mobileHeader >> H.map Menu >> Frame.mapLayout

mobileAddress : FrameModel app appMsg -> Html (FrameMsg appMsg)
mobileAddress = Menu.navAddress >> H.map Menu >> Frame.mapLayout

navHeader : FrameModel app appMsg -> Html (FrameMsg appMsg)
navHeader = Menu.navHeader >> H.map Menu >> Frame.mapLayout

navAddress : FrameModel app appMsg -> Html (FrameMsg appMsg)
navAddress = Menu.navAddress >> H.map Menu >> Frame.mapLayout

navBody : FrameModel app appMsg -> Html (FrameMsg appMsg)
navBody = Menu.navBody >> H.map Menu >> Frame.mapLayout

navFooter : FrameModel app appMsg -> Html (FrameMsg appMsg)
navFooter = Menu.navFooter >> H.map Menu >> Frame.mapLayout
