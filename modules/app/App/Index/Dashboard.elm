module GettoCodes.App.Index.Dashboard exposing
  ( Msg
  , init
  , encodeQuery
  , decodeQuery
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  )
import GettoCodes.App.Index.Model as Model
import GettoCodes.App.Index.Dashboard.Html as Html
import GettoCodes.App.Index.Dashboard.View as View
import GettoCodes.Layout.Frame as Frame
import GettoCodes.I18n.App.Index as I18n
import GettoCodes.I18n.App as AppI18n
import GettoCodes.Extension.Href.Home as Home

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type Msg
  = HelloWorld

signature = "dashboard"

init : Frame.InitModel -> ( Model.Dashboard, Model.Transition Msg )
init model =
  ( ()
  , T.none
  )

encodeQuery : Model.Dashboard -> QueryEncode.Value
encodeQuery model = QueryEncode.null

decodeQuery : List String -> QueryDecode.Value -> Model.Dashboard -> Model.Dashboard
decodeQuery names value model = model

encodeStore : Model.Dashboard -> Encode.Value
encodeStore model = Encode.null

decodeStore : Decode.Value -> Model.Dashboard -> Model.Dashboard
decodeStore value model = model

subscriptions : Model.Dashboard -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model.Dashboard -> ( Model.Dashboard, Model.Transition Msg )
update msg model =
  case msg of
    HelloWorld -> ( model, T.none )

contents : Model.Frame -> List (Html Msg)
contents model =
  [ model |> dashboard
  ]

dashboard : Model.Frame -> Html Msg
dashboard model =
  H.section [ A.class "dashboard" ]
    [ H.dl []
      [ H.dd []
        [ model |> example
        ]
      ]
    ]

example : Model.Frame -> Html Msg
example model = L.lazy
  (\static -> Html.example
    { data = View.example
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
