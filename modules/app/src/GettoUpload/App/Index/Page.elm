module GettoUpload.App.Index.Page exposing ( main )
import GettoUpload.App.Index.Dashboard as Dashboard
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Frame.Page as Layout
import GettoUpload.Layout.Command.Search as Search
import GettoUpload.Layout.Command.Store  as Store

import Getto.Command.Transition as Transition
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode
import Url exposing ( Url )
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

type alias FrameModel = Frame.Model Layout.Model Model Msg
type alias Command    = Transition.Command FrameModel Msg
type alias Model =
  { dashboard : Dashboard.Model
  }

type alias FrameMsg = Frame.Msg Layout.Msg Msg
type Msg
  = Dashboard Dashboard.Msg

setup : Frame.SetupApp Layout.Model Model Msg
setup =
  { search = search
  , store  = store
  , init   = init
  }

init : Frame.InitModel -> ( Model, Command )
init model =
  Transition.compose Transition.batch Model
    (model |> Dashboard.init |> Transition.mapCommand Dashboard)

search : Search.Init Model FrameModel Msg
search =
  ( \model ->
    [ ( "dashboard", model.dashboard |> Dashboard.search )
    ] |> QueryEncode.object
  , \value model ->
    Transition.compose Transition.batch Model
      (model.dashboard |> Dashboard.searchChanged ["dashboard"] value |> Transition.mapCommand Dashboard)
  )

store : Store.Init Model
store =
  ( \model ->
    [ ( "dashboard", model.dashboard |> Dashboard.store )
    ] |> Encode.object
  , \value model ->
    Model
      (model.dashboard |> Dashboard.storeChanged (value |> SafeDecode.valueAt ["dashboard"]))
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.dashboard |> Dashboard.subscriptions |> Sub.map Dashboard
  ] |> Sub.batch

update : Msg -> Model -> ( Model, Command )
update message =
  case message of
    Dashboard msg ->
      Transition.update
        .dashboard (\dashboard m -> { m | dashboard = dashboard })
        (Dashboard.update msg >> Transition.mapCommand Dashboard)

document : FrameModel -> Browser.Document FrameMsg
document model =
  { title = model |> Layout.documentTitle
  , body = [ model |> L.lazy content ]
  }

content : FrameModel -> Html FrameMsg
content model =
  H.section [ A.class "MainLayout" ] <|
    [ model |> Layout.mobileHeader |> Frame.mapLayout
    , model |> Layout.navAddress   |> Frame.mapLayout
    , H.article [] <|
      [ model |> Layout.articleHeader |> Frame.mapLayout ] ++
      ( model |> Dashboard.contents |> Frame.mapApp Dashboard ) ++
      [ model |> Layout.articleFooter |> Frame.mapLayout ]
    , model |> Layout.nav |> Frame.mapLayout
    ] ++
    ( model |> Dashboard.dialogs |> Frame.mapApp Dashboard )
