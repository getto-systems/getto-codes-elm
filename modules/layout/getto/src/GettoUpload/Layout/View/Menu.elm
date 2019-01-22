module GettoUpload.Layout.View.Menu exposing
  ( Side
  , Breadcrumb
  , side
  , breadcrumb
  )
import GettoUpload.Layout.Storage.Menu as MenuStorage
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

side : MenuModel.Menu -> ( { a | path : String }, { b | roles : List String }, { c | menu : MenuStorage.Model } ) -> List Side
side menu (static,credential,storage) =
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
        if storage.menu |> MenuStorage.isCollapsed group
          then []
          else
            items
            |> List.map
              (\item ->
                let
                  info = item |> MenuModel.info
                in
                  { isActive = (info.href == static.path) -- TODO || item |> MenuModel.children |> isActive
                  , title    = info.href |> I18n.title
                  , href     = info.href
                  , icon     = info.icon
                  , badge    = Nothing -- TODO api |> getter
                  }
              )
      }
    )

breadcrumb : MenuModel.Menu -> { a | path : String } -> Maybe Breadcrumb
breadcrumb menu static =
  let
    find parent =
      List.foldl
        (\item acc ->
          let
            info = (item |> MenuModel.info)
            push  stack = info :: stack
            found stack = ( True,  stack |> push )
            into  stack = ( False, stack |> push )
          in
            case acc of
              ( True, _ ) -> acc
              ( False, stack ) ->
                if info.href == static.path
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

toBreadcrumb : String -> ( Bool, List MenuModel.Info ) -> Maybe Breadcrumb
toBreadcrumb group result =
  case result of
    (True,entries) ->
      Just
        ( group |> I18n.menu
        , entries
          |> List.reverse
          |> List.map
            (\info ->
              { title = info.href |> I18n.title
              , href  = info.href
              , icon  = info.icon
              }
            )
        )
    _ -> Nothing
