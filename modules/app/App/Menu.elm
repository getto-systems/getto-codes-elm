module GettoCodes.App.Menu exposing
  ( menu
  )
import GettoCodes.App.Href as AppHref
import GettoCodes.App.Data.Upload.Href as UploadHref
import GettoCodes.App.System.Href as SystemHref
import GettoCodes.View.Menu as Menu exposing ( Menu )
import GettoCodes.View.Icon as IconView

menu : Menu
menu =
  [ ( "main"
    , [ Menu.item (IconView.fas "home") AppHref.index []
      ]
    )
  , ( "upload"
    , [ Menu.item (IconView.fas "file") UploadHref.list
        [ Menu.item (IconView.edit) UploadHref.new []
        , Menu.item (IconView.edit) UploadHref.edit_ []
        ]
      , Menu.item (IconView.fas "file") UploadHref.list_edit []
      ]
    )
  , ( "system"
    , [ Menu.item (IconView.fas "user") SystemHref.profile []
      ]
    )
  ]
