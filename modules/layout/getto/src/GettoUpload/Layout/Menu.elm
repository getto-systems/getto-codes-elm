module GettoUpload.Layout.Menu exposing
  ( menu
  )
import GettoUpload.Layout.Menu.Model exposing ( Menu )
import GettoUpload.Layout.Href.Home as Home

menu : Menu
menu =
  [ ( "main", Home.menu_ )
  , ( "data", Home.menu_ )
  ]
