module GettoUpload.App.Index.Page exposing ( main )
import GettoUpload.App.Index.Dashboard as Dashboard
import GettoUpload.Layout.Frame as Frame
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
  { init          = Frame.init (search,store) init
  , subscriptions = Frame.subscriptions subscriptions
  , onUrlRequest  = Frame.onUrlRequest
  , onUrlChange   = Frame.onUrlChange
  , update        = Frame.update update
  , view          = document
  }

type alias FrameModel = Frame.Model Model Msg
type alias Update = Transition.Update FrameModel Msg
type alias Model =
  { dashboard : Dashboard.Model
  }

type alias FrameMsg = Frame.Msg Msg
type Msg
  = Dashboard Dashboard.Msg

init : Frame.InitModel -> ( Model, Update )
init model =
  Transition.compose Transition.batch Model
    (model |> Dashboard.init |> Transition.mapUpdate Dashboard)

search : Search.Init Model FrameModel Msg
search =
  ( \model ->
    [ ( "dashboard", model.dashboard |> Dashboard.search )
    ] |> QueryEncode.object
  , \value model ->
    Transition.compose Transition.batch Model
      (model.dashboard |> Dashboard.searchChanged ["dashboard"] value |> Transition.mapUpdate Dashboard)
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

update : Msg -> Model -> ( Model, Update )
update message =
  case message of
    Dashboard msg ->
      Transition.update
        .dashboard (\dashboard m -> { m | dashboard = dashboard })
        (Dashboard.update msg >> Transition.mapUpdate Dashboard)

document : FrameModel -> Browser.Document FrameMsg
document model =
  { title = model |> Frame.documentTitle
  , body = [ model |> L.lazy content ]
  }

content : FrameModel -> Html FrameMsg
content model =
  H.section [ A.class "MainLayout" ] <|
    [ model |> Frame.mobileHeader
    , model |> Frame.navAddress
    , H.article [] <|
      [ model |> Frame.articleHeader ] ++
      ( model |> Dashboard.contents |> Frame.mapHtml Dashboard ) ++
      [ model |> Frame.articleFooter ]
    , model |> Frame.nav
    ] ++
    ( model |> Dashboard.dialogs |> Frame.mapHtml Dashboard )
