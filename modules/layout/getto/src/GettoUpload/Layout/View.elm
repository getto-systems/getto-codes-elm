module GettoUpload.Layout.View exposing
  ( documentTitle
  , mobileHeader
  , navAddress
  , articleHeader
  , articleFooter
  )
import GettoUpload.Layout.Model as Model
import GettoUpload.Layout.Menu as Menu
import GettoUpload.Layout.Href.Home as HomeHref
import GettoUpload.Layout.Version as Version
import GettoUpload.I18n.App as I18n

type ApiStatus
  = ApiNone

pageTitle : Model.Static -> String
pageTitle static = static.path |> I18n.title

documentTitle : Model.Static -> { pageTitle : String, company : String, title : String }
documentTitle static =
  { pageTitle = static |> pageTitle
  , company   = static.project.company
  , title     = static.project.title
  }

mobileHeader : Model.Static -> { company : String, title : String, sub : String }
mobileHeader static =
  { company = static.project.company
  , title   = static.project.title
  , sub     = static.project.sub
  }

navHeader : Model.Static -> { company : String, title : String, sub : String }
navHeader static =
  { company = static.project.company
  , title   = static.project.title
  , sub     = static.project.sub
  }

navAddress : Model.Layout -> Model.Plugin storage query -> { api : ApiStatus, roles : List String }
navAddress layout plugin =
  { api   = ApiNone -- ApiError httpError -- TODO : plugin |> apiStatus
  , roles = layout.credential.roles
  }

navBody : Model.Plugin storage query -> { menu : List Menu.Side }
navBody plugin =
  { menu =
    -- TODO : plugin |> Menu.side
    [ MenuHeader "MAIN" (MenuBadge 4) <|
      [ MenuItem True HomeHref.index "fas fa-home" (HomeHref.index |> pageTitle) (MenuBadge 4)
      ]
    , MenuBadge "DATA" MenuBadgeNone [] -- 閉じている時も badge をカウントするのを忘れないように
    ]
  }

navFooter : () -> { version : String }
navFooter _ =
  { version = Version.version
  }

articleHeader : Model.Static -> Model.Plugin storage query -> { pageTitle : String, breadcrumb : List Menu.Breadcrumb }
articleHeader static plugin =
  { pageTitle = static |> pageTitle
  , breadcrumb =
    -- TODO : plugin |> Menu.breadcrumb
    [ BreadcrumbHeader "MAIN"
    , BreadcrumbEntry "fas fa-home" HomeHref.index (HomeHref.index |> pageTitle)
    ]
  }

articleFooter : () -> { copyright : String }
articleFooter _ =
  { copyright = Version.copyright
  }
