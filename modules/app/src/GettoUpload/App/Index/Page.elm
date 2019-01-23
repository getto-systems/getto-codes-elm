module GettoUpload.App.Index.Page exposing ( main )
import GettoUpload.App.Index.Model as Model
import GettoUpload.App.Index.Dashboard as Dashboard
import GettoUpload.Layout.Frame as Frame

import Browser
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

main = Browser.application
  { init          = Frame.init Model.init init
  , subscriptions = Frame.subscriptions
  , onUrlRequest  = Frame.onUrlRequest
  , onUrlChange   = Frame.onUrlChange
  , update        = Frame.update update
  , view          = document
  }

type alias Model = Model.Model App
type alias App = {}

type alias Msg = Model.Msg AppMsg
type AppMsg
  = Dashboard Dashboard.Msg

init : Model.Init -> ( App, Model.Init )
init model =
  ( {}
  , model
  )

update : AppMsg -> Model -> Model
update msg model =
  case msg of
    Dashboard sub -> model |> Dashboard.update sub
      |> (\m -> model)

document : Model -> Browser.Document Msg
document model =
  { title = model |> Frame.documentTitle
  , body = [ model |> L.lazy content ]
  }

content : Model -> Html Msg
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
