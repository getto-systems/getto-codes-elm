module GettoUpload.App.Upload.Edit.Page exposing ( main )
import GettoUpload.App.Upload.Edit.Data   as Data
import GettoUpload.App.Upload.Edit.Info   as Info
import GettoUpload.App.Upload.Edit.Detail as Detail
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Page.Page as Layout

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Json.SafeDecode as SafeDecode

import Browser
import Json.Encode as Encode
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
type alias FrameMsg = Frame.Msg Layout.Msg Msg

type alias Model =
  { data   : Data.Model
  , info   : Info.Model
  , detail : Detail.Model
  }

type Msg
  = Data Data.Msg
  | Info Info.Msg
  | Detail Detail.Msg

setup : Frame.SetupApp Layout.Model Model Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( "data",   model.data   |> Data.encodeStore )
      , ( "info",   model.info   |> Info.encodeStore   model.data )
      , ( "detail", model.detail |> Detail.encodeStore model.data )
      ]
    , \value model ->
      let
        data = model.data |> Data.decodeStore (value |> SafeDecode.valueAt ["data"])
      in
        Model
          data
          ( model.info   |> Info.decodeStore   data (value |> SafeDecode.valueAt ["info"]) )
          ( model.detail |> Detail.decodeStore data (value |> SafeDecode.valueAt ["detail"]) )
    )
  , search =
    ( \model -> model.data |> Data.encodeQuery
    , \value model -> { model | data = model.data |> Data.decodeQuery [] value }
    )
  , init = init
  }

init : Frame.InitModel -> ( Model, FrameTransition )
init model =
  T.compose3 Model
    (model |> Data.init   |> T.map Data)
    (model |> Info.init   |> T.map Info)
    (model |> Detail.init |> T.map Detail)

subscriptions : Model -> Sub Msg
subscriptions model =
  [ model.data   |> Data.subscriptions   |> Sub.map Data
  , model.info   |> Info.subscriptions   |> Sub.map Info
  , model.detail |> Detail.subscriptions |> Sub.map Detail
  ] |> Sub.batch

data_   = T.prop .data   (\v m -> { m | data   = v })
info_   = T.prop .info   (\v m -> { m | info   = v })
detail_ = T.prop .detail (\v m -> { m | detail = v })

update : Msg -> Model -> ( Model, FrameTransition )
update message model =
  case message of
    Data   msg -> model |> T.update data_   (Data.update msg >> T.map Data)
    Info   msg -> model |> T.update info_   (Info.update model.data msg   >> T.mapBatch (Data,Info))
    Detail msg -> model |> T.update detail_ (Detail.update model.data msg >> T.mapBatch (Data,Detail))

document : FrameModel -> Browser.Document FrameMsg
document model =
  { title = model |> Layout.documentTitle
  , body = [ L.lazy content model ]
  }

content : FrameModel -> Html FrameMsg
content model =
  H.section [ A.class "MainLayout" ] <| List.concat
    [ [ model |> Layout.mobileHeader
      , model |> Layout.mobileAddress
      , H.article [] <| List.concat
        [ [ H.header []
            [ model |> Layout.articleHeader
            , model |> Layout.breadcrumb
            ]
          ]
        , [ H.section [ "edit" |> A.class ] <| List.concat
            [ model |> Info.contents   |> Frame.mapApp Info
            , model |> Detail.contents |> Frame.mapApp Detail
            ]
          ]
        , [ model |> Layout.articleFooter ]
        ]
      , H.nav []
        [ model |> Layout.navHeader
        , model |> Layout.navAddress
        , model |> Layout.nav
        , model |> Layout.navFooter
        ]
      ]
    , model |> Info.dialogs   |> Frame.mapApp Info
    , model |> Detail.dialogs |> Frame.mapApp Detail
    ]
