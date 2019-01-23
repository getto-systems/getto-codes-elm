module GettoUpload.Layout.Frame.Menu.View exposing
  ( Side
  , Breadcrumb
  , MenuI18n
  , side
  , breadcrumb
  )
import GettoUpload.Layout.Frame.Menu.Store as MenuStore
import GettoUpload.Layout.Frame.Menu.Http  as MenuHttp
import GettoUpload.Layout.Command.Static     as Static
import GettoUpload.Layout.Command.Credential as Credential
import GettoUpload.Layout.Menu as Menu
import GettoUpload.Layout.Fa as Fa

import Dict exposing ( Dict )

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

type alias SideAllow = List String -> ( String, List Menu.Item ) -> Bool
type alias SideBadge = Dict String (MenuHttp.Model -> Int)
type alias SideModel =
  { page       : Static.Page
  , credential : Credential.Model
  , store      : MenuStore.Model
  , http       : MenuHttp.Model
  }

side : MenuI18n -> Menu.Menu -> SideAllow -> SideBadge -> SideModel -> List Side
side i18n menu allow badge model =
  menu
  |> List.filter (allow (model.credential |> Credential.roles))
  |> List.map
    (\(group,items) ->
      let
        itemBadge item = badge |> Dict.get (item |> Menu.href) |> Maybe.map (\f -> model.http |> f)
        sum list =
          if list |> List.isEmpty
            then Nothing
            else list |> List.sum |> Just
      in
        { title = group |> i18n.menu
        , badge = items |> List.filterMap itemBadge |> sum
        , items =
          if (model.store |> MenuStore.isCollapsed group) && (items |> List.any (isActive model.page) |> not)
            then []
            else items |> List.map
              (\item ->
                { isActive = item |> isActive model.page
                , title    = item |> Menu.href |> i18n.title
                , href     = item |> Menu.href
                , icon     = item |> Menu.icon
                , badge    = item |> itemBadge
                }
              )
        }
    )

isActive : Static.Page -> Menu.Item -> Bool
isActive page item =
  (item |> isMatch page) ||
  (item |> Menu.children |> List.any (isActive page))

isMatch : Static.Page -> Menu.Item -> Bool
isMatch page = Menu.href >> (==) page.path

breadcrumb : MenuI18n -> Menu.Menu -> Static.Page -> Maybe Breadcrumb
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
                    case item |> Menu.children |> find (stack |> into) of
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

toBreadcrumb : MenuI18n -> String -> ( Bool, List Menu.Item ) -> Maybe Breadcrumb
toBreadcrumb i18n group result =
  case result of
    (True,entries) ->
      Just
        ( group |> i18n.menu
        , entries
          |> List.reverse
          |> List.map
            (\item ->
              { title = item |> Menu.href |> i18n.title
              , href  = item |> Menu.href
              , icon  = item |> Menu.icon
              }
            )
        )
    _ -> Nothing
