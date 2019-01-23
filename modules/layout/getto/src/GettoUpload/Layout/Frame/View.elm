module GettoUpload.Layout.Frame.View exposing
  ( documentTitle
  , mobileHeader
  , navAddress
  , articleHeader
  , articleFooter
  )
import GettoUpload.Layout.Frame.Menu as Menu
import GettoUpload.Layout.Frame.Menu.View  as MenuView
import GettoUpload.Layout.Frame.Menu.Store as MenuStore
import GettoUpload.Layout.Frame.Menu.Http  as MenuHttp
import GettoUpload.Layout.Command.Static     as Static
import GettoUpload.Layout.Command.Credential as Credential
import GettoUpload.Layout.Version as Version
import GettoUpload.I18n.App as I18n

type ApiStatus
  = ApiNone

documentTitle : ( Static.Page, Static.Project ) -> { pageTitle : String, company : String, title : String }
documentTitle (page,project) =
  { pageTitle = page |> pageTitle
  , company   = project.company
  , title     = project.title
  }

mobileHeader : Static.Project -> { company : String, title : String, sub : String }
mobileHeader project =
  { company = project.company
  , title   = project.title
  , sub     = project.sub
  }

navHeader : Static.Project -> { company : String, title : String, sub : String }
navHeader project =
  { company = project.company
  , title   = project.title
  , sub     = project.sub
  }

navAddress : Credential.Model -> { api : ApiStatus, roles : List String }
navAddress credential =
  { api   = ApiNone -- ApiError httpError -- TODO : plugin |> apiStatus
  , roles = credential |> Credential.roles
  }

navBody : { page : Static.Page, credential : Credential.Model, store : MenuStore.Model, http : MenuHttp.Model } -> { menu : List MenuView.Side }
navBody model =
  { menu = model |> MenuView.side menuI18n Menu.menu Menu.allow Menu.badges
  }

navFooter : () -> { version : String }
navFooter _ =
  { version = Version.version
  }

articleHeader : Static.Page -> { pageTitle : String, breadcrumb : Maybe MenuView.Breadcrumb }
articleHeader page =
  { pageTitle  = page |> pageTitle
  , breadcrumb = page |> MenuView.breadcrumb menuI18n Menu.menu
  }

articleFooter : () -> { copyright : String }
articleFooter _ =
  { copyright = Version.copyright
  }


pageTitle : Static.Page -> String
pageTitle page = page.path |> I18n.title

menuI18n : MenuView.MenuI18n
menuI18n =
  { menu  = I18n.menu
  , title = I18n.title
  }
