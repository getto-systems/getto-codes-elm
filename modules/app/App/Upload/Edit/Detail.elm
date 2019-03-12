module GettoUpload.App.Upload.Edit.Detail exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  )
import GettoUpload.App.Upload.Edit.Model as Model
import GettoUpload.App.Upload.Edit.Detail.View as View
import GettoUpload.App.Upload.Edit.Detail.Html as Html
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload as I18n
import GettoUpload.I18n.Http as HttpI18n

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type Msg
  = Edit
  | Cancel
  | Change
  | Request
  | Input      (View.Prop String)       String
  | Toggle     (View.Prop (Set String)) String
  | Resolve    (View.Prop String)       (Conflict.Resolve String)
  | ResolveSet (View.Prop (Set String)) (Conflict.Resolve (Set String))
  | StateChanged (HttpView.Migration View.Response)

signature = "detail"

put : Http.Tracker Model.Frame View.Response
put = Http.tracker "put" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      m    = model |> Frame.app |> .detail
    in
      Http.put
        { url      = "upload/:id/detail" |> Api.url ( data |> Model.pathInfo )
        , headers  = model  |> Api.headers
        , params   = m.form |> View.params data.get
        , response = View.response
        , timeout  = 10 * 1000
        }

putTrack   = Http.track   signature put StateChanged
putRequest = Http.request signature put StateChanged


init : Frame.InitModel -> ( Model.Detail, Model.Transition Msg )
init model =
  ( { form = signature |> View.init
    , put  = HttpView.empty
    }
  , fill
  )

encodeStore : Model.Data -> Model.Detail -> Encode.Value
encodeStore data model = Encode.object
  [ ( data.id |> String.fromInt
    , model.form |> View.encodeForm
    )
  ]

decodeStore : Model.Data -> Decode.Value -> Model.Detail -> Model.Detail
decodeStore data value model =
  { model
  | form = model.form |> View.decodeForm
    (value |> SafeDecode.valueAt [data.id |> String.fromInt])
  }

subscriptions : Model.Detail -> Sub Msg
subscriptions model = putTrack

update : Model.Data -> Msg -> Model.Detail -> ( Model.Detail, ( Model.Transition Msg, Bool ) )
update data msg model =
  case msg of
    Edit    -> ( { model | form = model.form |> Edit.edit View.edit data.get }, ( fillAndStore,   False ) )
    Cancel  -> ( { model | form = model.form |> Edit.cancel },                  ( Frame.storeApp, False ) )
    Change  -> ( { model | form = model.form |> Edit.change },                  ( Frame.storeApp, False ) )
    Request -> ( { model | form = model.form |> Edit.commit },                  ( putRequest,     False ) )

    Input  prop value -> ( { model | form = model.form |> Form.set    prop value }, ( T.none, False ) )
    Toggle prop value -> ( { model | form = model.form |> Form.toggle prop value }, ( T.none, False ) )

    Resolve    prop mig -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( fillAndStore, False ) )
    ResolveSet prop mig -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( fillAndStore, False ) )

    StateChanged mig ->
      ( { model
        | put  = model.put  |> HttpView.update mig
        , form = model.form |> Edit.put mig
        }
      , ( T.none, True )
      )

fill : Model.Transition Msg
fill = Frame.app >> .detail >> .form >> Edit.fields >> Html.pairs >> Dom.fill

fillAndStore : Model.Transition Msg
fillAndStore = [ fill, Frame.storeApp ] |> T.batch


contents : Model.Frame -> List (Html Msg)
contents model =
  [ model |> detail
  ]

detail : Model.Frame -> Html Msg
detail model = L.lazy2
  (\data m -> Html.detail
    { view = m.form |> View.view
    , get  = data.get
    , put  = m.put
    , options =
      { gender =
        [ ( "", "please-select" |> AppI18n.form )
        , ( "male",   "male"   |> I18n.gender )
        , ( "female", "female" |> I18n.gender )
        , ( "other",  "other"  |> I18n.gender )
        ]
      , quality =
        [ ( "high", "high" |> I18n.quality )
        , ( "low",  "low"  |> I18n.quality )
        ]
      , roles =
        [ ( "admin",  "admin"  |> AppI18n.role )
        , ( "upload", "upload" |> AppI18n.role )
        ]
      }
    , msg =
      { put        = Request
      , input      = Input
      , toggle     = Toggle
      , resolve    = Resolve
      , resolveSet = ResolveSet
      , change     = Change
      , edit       = Edit
      , cancel     = Cancel
      }
    , i18n =
      { title = I18n.title
      , field = I18n.field
      , error = I18n.error
      , form  = AppI18n.form
      , http  = HttpI18n.error
      }
    }
  )
  (model |> Frame.app |> .data)
  (model |> Frame.app |> .detail)
