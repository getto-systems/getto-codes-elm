module GettoUpload.Layout.Frame.Menu exposing
  ( menu
  )
import GettoUpload.Layout.Menu as Menu exposing ( Menu )
import GettoUpload.Layout.Href.Home as Home
import GettoUpload.Layout.Fa as Fa

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
