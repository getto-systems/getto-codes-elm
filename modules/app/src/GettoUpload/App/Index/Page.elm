module GettoUpload.App.Index.Page exposing ( main )
import GettoUpload.App.Index.Dashboard as Dashboard
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Command.Search as Search
import GettoUpload.Layout.Command.Store  as Store

import Getto.Command.Transit as Transit
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
  { init          = Frame.init (search,store) construct init
  , subscriptions = Frame.subscriptions subscriptions
  , onUrlRequest  = Frame.onUrlRequest
  , onUrlChange   = Frame.onUrlChange
  , update        = Frame.update update
  , view          = document
  }

type alias FrameModel = Frame.Model Model Msg
type alias Model =
  { dashboard : Dashboard.Model
  }

type alias FrameMsg = Frame.Msg Msg
type Msg
  = Dashboard Dashboard.Msg

construct : Frame.InitModel -> Model
construct model =
  { dashboard = model |> Dashboard.construct
  }

init : FrameModel -> ( FrameModel, Cmd Msg )
init = Transit.none
  >> Transit.andThen (Dashboard.init >> Transit.map Dashboard)

search : Search.Init Model Msg
search =
  ( \model ->
    [ ( "dashboard", model.dashboard |> Dashboard.search )
    ] |> QueryEncode.object
  , \value model ->
    Transit.compose Model
      (model.dashboard |> Dashboard.searchChanged ["dashboard"] value |> Transit.map Dashboard)
  )

store : Store.Init Model
store =
  ( \model ->
    [ ( "dashboard", model.dashboard |> Dashboard.store )
    ] |> Encode.object
  , \value model ->
    { model
    | dashboard = model.dashboard |> Dashboard.storeChanged (value |> SafeDecode.valueAt ["dashboard"])
    }
  )

subscriptions : FrameModel -> Sub Msg
subscriptions model =
  [ Sub.none
  , model |> Dashboard.subscriptions |> Sub.map Dashboard
  ] |> Sub.batch

update : Msg -> FrameModel -> ( FrameModel, Cmd Msg )
update message model =
  case message of
    Dashboard msg -> model |> Dashboard.update msg |> Transit.map Dashboard

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
