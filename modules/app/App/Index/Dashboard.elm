module GettoUpload.App.Index.Dashboard exposing
  ( Model
  , Msg
  , init
  , store
  , storeChanged
  , query
  , queryChanged
  , fill
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Index.Dashboard.Html as Html
import GettoUpload.App.Index.Dashboard.View as View
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.I18n.App.Index.Dashboard as DashboardI18n
import GettoUpload.I18n.App as I18n
import GettoUpload.Extension.Href.Home as Home

import Getto.Command.Transition as Transition exposing ( Transition )
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
type alias Model =
  { signature : String
  }

type Msg
  = HelloWorld

init : String -> Frame.InitModel -> ( Model, FrameTransition a )
init signature model =
  ( { signature = signature
    }
  , Transition.none
  )

store : Model -> Encode.Value
store model = Encode.null

storeChanged : Decode.Value -> Model -> Model
storeChanged value model = model

query : Model -> QueryEncode.Value
query model = QueryEncode.empty

queryChanged : List String -> QueryDecode.Value -> Model -> Model
queryChanged names value model = model

fill : Model -> List ( String, String )
fill model = []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    HelloWorld -> ( model, Transition.none )

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
      { title = I18n.title
      , name  = DashboardI18n.title
      }
    }
  )
  (model |> Frame.static)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
