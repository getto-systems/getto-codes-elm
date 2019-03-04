module GettoUpload.App.Index.Dashboard exposing
  ( Model
  , Msg
  , init
  , encodeQuery
  , decodeQuery
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Index.Dashboard.Html as Html
import GettoUpload.App.Index.Dashboard.View as View
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.I18n.App.Index.Dashboard as I18n
import GettoUpload.I18n.App as AppI18n
import GettoUpload.Extension.Href.Home as Home

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | dashboard : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model = ()

type Msg
  = HelloWorld

signature = "dashboard"

init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( ()
  , T.none
  )

encodeQuery : Model -> QueryEncode.Value
encodeQuery model = QueryEncode.empty

decodeQuery : List String -> QueryDecode.Value -> Model -> Model
decodeQuery names value model = model

encodeStore : Model -> Encode.Value
encodeStore model = Encode.null

decodeStore : Decode.Value -> Model -> Model
decodeStore value model = model

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    HelloWorld -> ( model, T.none )

contents : FrameModel a -> List (Html Msg)
contents model =
  [ model |> dashboard
  ]

dashboard : FrameModel a -> Html Msg
dashboard model =
  H.section [ A.class "dashboard" ]
    [ H.dl []
      [ H.dd []
        [ model |> example
        ]
      ]
    ]

example : FrameModel a -> Html Msg
example model = L.lazy
  (\static -> Html.example
    { name = "example"
    , data = View.example
      { current = 3
      , target = 20
      }
    , page = Home.index
    , i18n =
      { title = AppI18n.title
      , name  = I18n.title
      }
    }
  )
  (model |> Frame.static)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
