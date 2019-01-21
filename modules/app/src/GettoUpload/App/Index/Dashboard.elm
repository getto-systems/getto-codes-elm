module GettoUpload.App.Index.Dashboard exposing
  ( Model
  , Msg
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Index.Model as Model
import GettoUpload.Layout.Href.Home as HomeHref

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias Model a = Model.Model a

type Msg
  = HelloWorld

update : Msg -> Model a -> Model a
update msg model =
  case msg of
    HelloWorld -> model

contents : Model a -> List (Html Msg)
contents model =
  [ model |> dashboard
  ]

dashboard : Model a -> Html Msg
dashboard model =
  H.section [ A.class "dashboard" ]
    [ H.dl []
      [ H.dd []
        [ H.section []
          [ H.h2 [] [ "Subject" |> H.text ]
          , H.p []
            [ H.em []
              [ "3" |> H.text
              , H.small [] [ H.small [] [ "million" |> H.text ] ]
              ]
            , " " |> H.text
            , H.em [] [ "/" |> H.text ]
            , " " |> H.text
            , H.em []
              [ H.small [] [ "20" |> H.text ]
              , H.small [] [ H.small [] [ "million" |> H.text ] ]
              ]
            ]
          , H.meter [ A.value "3", A.max "20" ] [ "15%" |> H.text ]
          , H.footer []
            [ H.a [ A.href HomeHref.index ]
              [ H.i [ A.class "fas fa-user" ] []
              , " " |> H.text
              , "User" |> H.text
              ]
            ]
          ]
        ]
      ]
    ]

dialogs : Model a -> List (Html Msg)
dialogs model = []
