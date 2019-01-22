module GettoUpload.Layout.View.Menu exposing
  ( Side
  , Breadcrumb
  , side
  , breadcrumb
  )
import GettoUpload.Layout.Store.Menu as MenuStore
import GettoUpload.Layout.Menu as Menu
import GettoUpload.Layout.Menu.Model as MenuModel
import GettoUpload.Layout.Model as Model
import GettoUpload.Layout.Fa as Fa
import GettoUpload.I18n.App as I18n

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

side : MenuModel.Menu -> ( Model.Page, Model.Credential, MenuStore.Model ) -> List Side
side menu (page,credential,storage) =
  menu
  |> List.filter
    (\(group,_) ->
      (group == "home") ||
      (group == "system") ||
      (credential.roles |> List.member "admin") ||
      (credential.roles |> List.member group)
    )
  |> List.map
    (\(group,items) ->
      { title = group |> I18n.menu
      , badge = Nothing -- TODO items |> map (api |> getter) |> sum
      , items =
        if storage |> MenuStore.isCollapsed group
          then []
          else items |> List.map
            (\item ->
              { isActive = item |> isActive page
              , title    = item |> MenuModel.href |> I18n.title
              , href     = item |> MenuModel.href
              , icon     = item |> MenuModel.icon
              , badge    = Nothing -- TODO api |> getter
              }
            )
      }
    )

isActive : Model.Page -> MenuModel.Item -> Bool
isActive page item =
  (item |> isMatch page) ||
  (item |> MenuModel.children |> List.any (isActive page))

isMatch : Model.Page -> MenuModel.Item -> Bool
isMatch page = MenuModel.href >> (==) page.path

breadcrumb : MenuModel.Menu -> Model.Page -> Maybe Breadcrumb
breadcrumb menu page =
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
        |> toBreadcrumb group
      )
    |> List.head

toBreadcrumb : String -> ( Bool, List MenuModel.Item ) -> Maybe Breadcrumb
toBreadcrumb group result =
  case result of
    (True,entries) ->
      Just
        ( group |> I18n.menu
        , entries
          |> List.reverse
          |> List.map
            (\item ->
              { title = item |> MenuModel.href |> I18n.title
              , href  = item |> MenuModel.href
              , icon  = item |> MenuModel.icon
              }
            )
        )
    _ -> Nothing
