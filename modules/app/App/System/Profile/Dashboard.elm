module GettoCodes.App.System.Profile.Dashboard exposing
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
import GettoCodes.App.System.Profile.I18n as I18n
import GettoCodes.App.System.Profile.Model as Model
import GettoCodes.App.System.Profile.Dashboard.Html as Html
import GettoCodes.Layout.Frame as Frame

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type Msg
  = Logout

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
    Logout -> ( model, Frame.logout )

contents : Model.Frame -> List (Html Msg)
contents model =
  [ model |> dashboard
  ]

dashboard : Model.Frame -> Html Msg
dashboard model =
  H.section [ A.class "dashboard" ]
    [ H.dl []
      [ H.dd []
        [ model |> logout
        ]
      ]
    ]

logout : Model.Frame -> Html Msg
logout model = L.lazy
  (\static -> Html.logout
    { msg =
      { logout = Logout
      }
    , i18n =
      { button = I18n.button
      }
    }
  )
  (model |> Frame.static)
