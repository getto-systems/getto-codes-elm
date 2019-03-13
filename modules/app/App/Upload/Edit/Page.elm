module GettoUpload.App.Upload.Edit.Page exposing ( main )
import GettoUpload.App.Upload.Edit.Model      as Model
import GettoUpload.App.Upload.Edit.Data       as Data
import GettoUpload.App.Upload.Edit.Info       as Info
import GettoUpload.App.Upload.Edit.Detail     as Detail
import GettoUpload.App.Upload.Edit.Complete   as Complete
import GettoUpload.App.Upload.Edit.Unregister as Unregister
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Page.Page as Layout

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode

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

type Msg
  = Data Data.Msg
  | Info Info.Msg
  | Detail Detail.Msg
  | Complete Complete.Msg
  | Unregister Unregister.Msg

setup : Frame.SetupApp Layout.Model Model.Page Msg
setup =
  { store =
    ( \model -> Encode.object
      [ ( model.data.id |> String.fromInt
        , [ ( "data",       model.data       |> Data.encodeStore )
          , ( "info",       model.info       |> Info.encodeStore )
          , ( "detail",     model.detail     |> Detail.encodeStore )
          , ( "complete",   model.complete   |> Complete.encodeStore )
          , ( "unregister", model.unregister |> Unregister.encodeStore )
          ] |> Encode.object
        )
      ]
    , \value model ->
      let
        obj = value |> SafeDecode.valueAt [ model.data.id |> String.fromInt ]
      in
        Model.Page
          ( model.data       |> Data.decodeStore       (obj |> SafeDecode.valueAt ["data"]) )
          ( model.info       |> Info.decodeStore       (obj |> SafeDecode.valueAt ["info"]) )
          ( model.detail     |> Detail.decodeStore     (obj |> SafeDecode.valueAt ["detail"]) )
          ( model.complete   |> Complete.decodeStore   (obj |> SafeDecode.valueAt ["complete"]) )
          ( model.unregister |> Unregister.decodeStore (obj |> SafeDecode.valueAt ["unregister"]) )
    )
  , search =
    ( \model -> model.data |> Data.encodeQuery
    , \value model -> { model | data = model.data |> Data.decodeQuery [] value }
    )
  , init = init
  }

init : Frame.InitModel -> ( Model.Page, Model.Transition Msg )
init model =
  T.compose5 Model.Page
    (model |> Data.init       |> T.map Data)
    (model |> Info.init       |> T.map Info)
    (model |> Detail.init     |> T.map Detail)
    (model |> Complete.init   |> T.map Complete)
    (model |> Unregister.init |> T.map Unregister)

subscriptions : Model.Page -> Sub Msg
subscriptions model =
  [ model.data       |> Data.subscriptions       |> Sub.map Data
  , model.info       |> Info.subscriptions       |> Sub.map Info
  , model.detail     |> Detail.subscriptions     |> Sub.map Detail
  , model.complete   |> Complete.subscriptions   |> Sub.map Complete
  , model.unregister |> Unregister.subscriptions |> Sub.map Unregister
  ] |> Sub.batch

data_       = T.prop .data       (\v m -> { m | data       = v })
info_       = T.prop .info       (\v m -> { m | info       = v })
detail_     = T.prop .detail     (\v m -> { m | detail     = v })
complete_   = T.prop .complete   (\v m -> { m | complete   = v })
unregister_ = T.prop .unregister (\v m -> { m | unregister = v })

update : Msg -> Model.Page -> ( Model.Page, Model.Transition Msg )
update message model =
  case message of
    Data       msg -> model |> T.update data_       (Data.update msg       >> T.map Data)
    Unregister msg -> model |> T.update unregister_ (Unregister.update msg >> T.map Unregister)
    Info       msg -> model |> T.update info_     (Info.update   model.data msg >> Tuple.mapSecond (batch Info))
    Detail     msg -> model |> T.update detail_   (Detail.update model.data msg >> Tuple.mapSecond (batch Detail))
    Complete   msg -> model |> T.update complete_ (Complete.update          msg >> Tuple.mapSecond (batch Complete))

batch : (msg -> Msg) -> ( Transition Model.Frame msg, Bool ) -> Model.Transition Msg
batch msg (t,req) =
  [ t >> Cmd.map msg
  , if req
    then Data.getRequest >> Cmd.map Data
    else T.none
  ] |> T.batch

document : Model.Frame -> Browser.Document (Model.Msg Msg)
document model =
  { title = model |> Layout.documentTitle
  , body = [ L.lazy content model ]
  }

content : Model.Frame -> Html (Model.Msg Msg)
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
            [ model |> Info.contents       |> Frame.mapApp Info
            , model |> Detail.contents     |> Frame.mapApp Detail
            , model |> Complete.contents   |> Frame.mapApp Complete
            , model |> Unregister.contents |> Frame.mapApp Unregister
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
    , model |> Complete.dialogs   |> Frame.mapApp Complete
    , model |> Unregister.dialogs |> Frame.mapApp Unregister
    ]
