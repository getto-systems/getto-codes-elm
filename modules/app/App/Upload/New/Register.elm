module GettoUpload.App.Upload.New.Register exposing
  ( Model
  , Msg
  , init
  , query
  , queryChanged
  , store
  , storeChanged
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Upload.New.Register.View as View
import GettoUpload.App.Upload.New.Register.Html as Html
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload.New.Register as I18n
import GettoUpload.I18n.Http as HttpI18n
import GettoUpload.Extension.Href as Href
import GettoUpload.Extension.Href.Upload as Upload

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Http.Part as Part
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field

import Browser.Navigation as Navigation
import File exposing ( File )
import File.Select
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | register : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { form : View.Form
  , upload : Http.Model Upload Decode.Value
  }

type alias Upload =
  { id : Int
  }

type Msg
  = FieldInput (View.Prop String) String
  | FieldChange
  | FileRequest (View.Prop (List File))
  | FileSelect (View.Prop (List File)) File
  | UploadRequest
  | UploadStateChanged (HttpView.State Upload Decode.Value)

init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { form =
      { name = Field.init "register" "name" ""
      , text = Field.init "register" "text" []
      }
    , upload = Http.init
    }
  , fill
  )

fill : FrameModel a -> Cmd msg
fill = Frame.app >> .register >>
  (\m -> Dom.fill
    [ m.form.name |> Dom.string
    ]
  )

upload : Http.Request (FrameModel a) Upload Decode.Value
upload = Api.request
  (\(host,headers) ->
    Http.upload
      { url     = host ++ "upload"
      , headers = headers
      , params  = \model ->
        let
          m = model |> Frame.app |> .register
        in
          Part.object
            [ ( "name",  m.form.name |> Field.value |> Part.string )
            , ( "text" , m.form.text |> Field.value |> Part.list Part.file )
            ]
      , response =
        { header = HeaderDecode.map Upload
          ( HeaderDecode.at "x-upload-id" HeaderDecode.int )
        , body = Decode.value
        }
      , timeout = 30 * 1000
      }
  )

query : Model -> QueryEncode.Value
query model = QueryEncode.empty

queryChanged : List String -> QueryDecode.Value -> Model -> Model
queryChanged names value model = model

store : Model -> Encode.Value
store model = Encode.object
  [ ( "name", model.form.name |> Field.value |> Encode.string )
  ]

storeChanged : Decode.Value -> Model -> Model
storeChanged value model =
  { model
  | form =
    model.form
    |> View.change name_ ( value |> SafeDecode.at ["name"] (SafeDecode.string "") )
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track upload UploadStateChanged

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    FieldInput prop value ->
      ( { model | form = model.form |> View.change prop value }
      , Transition.none
      )
    FieldChange -> ( model, Frame.storeApp )

    FileRequest prop -> ( model, always ( FileSelect prop |> File.Select.file [] ) )
    FileSelect prop file ->
      ( { model | form = model.form |> View.change prop [file] }
      , Transition.none
      )

    UploadRequest -> ( model, Http.request upload UploadStateChanged )
    UploadStateChanged state ->
      ( { model | upload = model.upload |> Http.stateTo state }
      , case state of
        HttpView.Ready (Just (Ok res)) ->
          [ Frame.clearApp
          , always ( res |> HttpView.header |> .id |> Upload.edit |> Href.toString |> Navigation.load )
          ]
          |> Transition.batch
        _ -> Transition.none
      )

contents : FrameModel a -> List (Html Msg)
contents model =
  [ model |> list
  ]

list : FrameModel a -> Html Msg
list model =
  H.section [ A.class "edit" ]
    [ model |> register
    ]

name_ = View.prop .name (\v m -> { m | name = v })
text_ = View.prop .text (\v m -> { m | text = v })

register : FrameModel a -> Html Msg
register model = L.lazy
  (\m -> Html.register
    { title = "select-file"
    , form = m.form |> View.compose
      ( name_, [ m.form.name |> Field.blank "blank" ] )
      ( text_, [ m.form.text |> Field.empty "no-file" ] )
    , state = m.upload |> Http.state
    , msg =
      { upload = UploadRequest
      , input  = FieldInput
      , change = FieldChange
      , select = FileRequest
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
  (model |> Frame.app |> .register)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
