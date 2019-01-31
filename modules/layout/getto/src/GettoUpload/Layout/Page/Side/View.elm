module GettoUpload.Layout.Page.Side.View exposing
  ( Side
  , Breadcrumb
  , MenuI18n
  , menu
  , breadcrumb
  )
import GettoUpload.Layout.View.Menu as Menu
import GettoUpload.Layout.View.Icon as Icon exposing ( Icon )

import Dict exposing ( Dict )

type alias Side =
  { title     : String
  , badge     : Maybe Int
  , collapsed : Bool
  , items     : List SideEntry
  }

type alias SideEntry =
  { active : Bool
  , title  : String
  , href   : String
  , icon   : Icon
  , badge  : Maybe Int
  }

type alias Breadcrumb = ( String, List BreadcrumbEntry )

type alias BreadcrumbEntry =
  { title : String
  , icon  : Icon
  , href  : String
  }

type alias MenuI18n =
  { menu  : String -> String
  , title : String -> String
  }

type alias SideModel =
  { path       : String
  , roles      : List String
  , menu       : Menu.Menu
  , allow      : List String -> ( String, List Menu.Item ) -> Bool
  , collapsed  : String -> Bool
  , badge      : String -> Maybe Int
  , i18n       : MenuI18n
  }

menu : SideModel -> List Side
menu model =
  let
    badge item =
      item
      |> Menu.children
      |> List.foldl
        (badge >> (::))
        [item |> Menu.href |> model.badge]
      |> List.filterMap identity
      |> sum

    active item =
      (item |> match model.path) ||
      (item |> Menu.children |> List.any active)
  in
    model.menu
    |> List.filter (model.roles |> model.allow)
    |> List.map
      (\(group,items) ->
        let
          entries =
            items |> List.map
              (\item ->
                { active = item |> active
                , title  = item |> Menu.href |> model.i18n.title
                , href   = item |> Menu.href
                , icon   = item |> Menu.icon
                , badge  = item |> badge
                }
              )

          collapsed =
            (group |> model.collapsed) &&
            (entries |> List.any .active |> not)
        in
          { title     = group |> model.i18n.menu
          , badge     = entries |> List.filterMap .badge |> sum
          , collapsed = collapsed
          , items =
            if collapsed
              then []
              else entries
          }
      )

sum : List Int -> Maybe Int
sum list =
  if list |> List.isEmpty
    then Nothing
    else list |> List.sum |> Just

match : String -> Menu.Item -> Bool
match path = Menu.href >> (==) path

type alias BreadcrumbModel =
  { path : String
  , menu : Menu.Menu
  , i18n : MenuI18n
  }

breadcrumb : BreadcrumbModel -> Maybe Breadcrumb
breadcrumb model =
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
                if item |> match model.path
                  then stack |> found
                  else
                    case item |> Menu.children |> find (stack |> into) of
                      ( True,  result ) -> ( True,  result )
                      ( False, result ) -> ( False, result |> List.drop 1 )
        )
        parent
  in
    model.menu
    |> List.filterMap
      (\(group,items) ->
        items
        |> find (False,[])
        |> toBreadcrumb model.i18n group
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
