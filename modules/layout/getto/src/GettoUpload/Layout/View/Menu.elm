module GettoUpload.Layout.View.Menu exposing
  ( Side
  , Breadcrumb
  , MenuI18n
  , side
  , breadcrumb
  )
import GettoUpload.Layout.Command.Static     as Static
import GettoUpload.Layout.Command.Credential as Credential
import GettoUpload.Layout.Command.Store      as Store
import GettoUpload.Layout.Command.Search     as Search
import GettoUpload.Layout.Store.Menu as MenuStore
import GettoUpload.Layout.Menu as Menu
import GettoUpload.Layout.Menu.Model as MenuModel
import GettoUpload.Layout.Fa as Fa

type alias Side =
  { title : String
  , badge : Badge
  , items : List SideEntry
  }

type alias SideEntry =
  { isActive : Bool
  , title    : String
  , href     : String
  , icon     : Fa.Icon
  , badge    : Badge
  }

type alias Badge = Maybe Int

type alias Breadcrumb = ( String, List BreadcrumbEntry )

type alias BreadcrumbEntry =
  { title : String
  , icon  : Fa.Icon
  , href  : String
  }

type alias MenuI18n =
  { menu  : I18n
  , title : I18n
  }
type alias I18n = String -> String

side : MenuI18n -> MenuModel.Menu -> ( Static.Page, Credential.Model, MenuStore.Model ) -> List Side
side i18n menu (page,credential,store) =
  let
    roles = credential |> Credential.roles
  in
    menu
    |> List.filter
      (\(group,_) ->
        (group == "home") ||
        (group == "system") ||
        (roles |> List.member "admin") ||
        (roles |> List.member group)
      )
    |> List.map
      (\(group,items) ->
        { title = group |> i18n.menu
        , badge = Nothing -- TODO items |> map (api |> getter) |> sum
        , items =
          if (store |> MenuStore.isCollapsed group) && (items |> List.any (isActive page) |> not)
            then []
            else items |> List.map
              (\item ->
                { isActive = item |> isActive page
                , title    = item |> MenuModel.href |> i18n.title
                , href     = item |> MenuModel.href
                , icon     = item |> MenuModel.icon
                , badge    = Nothing -- TODO api |> getter
                }
              )
        }
      )

isActive : Static.Page -> MenuModel.Item -> Bool
isActive page item =
  (item |> isMatch page) ||
  (item |> MenuModel.children |> List.any (isActive page))

isMatch : Static.Page -> MenuModel.Item -> Bool
isMatch page = MenuModel.href >> (==) page.path

breadcrumb : MenuI18n -> MenuModel.Menu -> Static.Page -> Maybe Breadcrumb
breadcrumb i18n menu page =
  let
    find parent =
      List.foldl
        (\item acc ->
          let
            push  stack = item :: stack
            found stack = ( True,  stack |> push )
            into  stack = ( False, stack |> push )
          in
            case acc of
              ( True, _ ) -> acc
              ( False, stack ) ->
                if item |> isMatch page
                  then stack |> found
                  else
                    case item |> MenuModel.children |> find (stack |> into) of
                      ( True,  result ) -> ( True,  result )
                      ( False, result ) -> ( False, result |> List.drop 1 )
        )
        parent
  in
    menu
    |> List.filterMap
      (\(group,items) ->
        items
        |> find (False,[])
        |> toBreadcrumb i18n group
      )
    |> List.head

toBreadcrumb : MenuI18n -> String -> ( Bool, List MenuModel.Item ) -> Maybe Breadcrumb
toBreadcrumb i18n group result =
  case result of
    (True,entries) ->
      Just
        ( group |> i18n.menu
        , entries
          |> List.reverse
          |> List.map
            (\item ->
              { title = item |> MenuModel.href |> i18n.title
              , href  = item |> MenuModel.href
              , icon  = item |> MenuModel.icon
              }
            )
        )
    _ -> Nothing
