module GettoUpload.Layout.Frame.Menu exposing
  ( menu
  , allow
  , badges
  )
import GettoUpload.Layout.Frame.Menu.Http as Http
import GettoUpload.Layout.Menu as Menu exposing ( Menu )
import GettoUpload.Layout.Href.Home as Home
import GettoUpload.Layout.Fa as Fa

import Dict exposing ( Dict )

menu : Menu
menu =
  [ ( "main"
    , [ Menu.item (Fa.solid "home") Home.index []
      ]
    )
  , ( "data"
    , [ Menu.item (Fa.solid "home") Home.index []
      ]
    )
  ]

allow : List String -> ( String, List Menu.Item ) -> Bool
allow roles (group,_) =
  (group == "home") ||
  (group == "system") ||
  (roles |> List.member "admin") ||
  (roles |> List.member group)

badges : Dict String (Http.Model -> Int)
badges = Dict.fromList
  [ ( Home.index, always 4 ) -- TODO 本当は .index とか
  ]
