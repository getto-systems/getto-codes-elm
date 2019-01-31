module GettoUpload.Layout.Page.Side.View exposing
  ( Breadcrumb
  , Menu
  , BadgeState(..)
  , MenuI18n
  , badgeState
  , menu
  , breadcrumb
  )
import GettoUpload.Extension.View.Menu as Menu
import GettoUpload.Extension.View.Icon as Icon exposing ( Icon )
import GettoUpload.Extension.View.Http as HttpView
import GettoUpload.Extension.Href as Href exposing ( Href )


type alias Breadcrumb = ( String, List BreadcrumbItem )

type alias BreadcrumbItem =
  { title : String
  , icon  : Icon
  , href  : Href
  }

type alias Menu =
  { name      : String
  , title     : String
  , badge     : Maybe Int
  , collapsed : Bool
  , items     : List MenuItem
  }

type alias MenuItem =
  { active : Bool
  , title  : String
  , href   : Href
  , icon   : Icon
  , badge  : Maybe Int
  }

type BadgeState
  = NoProbrem
  | Loading
  | Failure String

type alias MenuI18n =
  { menu  : String -> String
  , title : String -> String
  }


type alias BadgeStateModel data =
  { state : HttpView.State data
  , i18n  : HttpView.Error -> String
  }

badgeState : BadgeStateModel data -> BadgeState
badgeState model =
  case model.state of
    HttpView.Empty      -> NoProbrem
    HttpView.Loading    -> Loading
    HttpView.Progress _ -> Loading
    HttpView.Success  _ -> NoProbrem
    HttpView.Failure error -> error |> model.i18n |> Failure


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
              { title = item |> Menu.href |> Href.path |> i18n.title
              , href  = item |> Menu.href
              , icon  = item |> Menu.icon
              }
            )
        )
    _ -> Nothing


type alias MenuModel =
  { path       : String
  , roles      : List String
  , menu       : Menu.Menu
  , allow      : List String -> ( String, List Menu.Item ) -> Bool
  , collapsed  : String -> Bool
  , badge      : Href -> Maybe Int
  , i18n       : MenuI18n
  }

menu : MenuModel -> List Menu
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
          collapsed = group |> model.collapsed

          entries =
            items |> List.map
              (\item ->
                { active = item |> active
                , title  = item |> Menu.href |> Href.path |> model.i18n.title
                , href   = item |> Menu.href
                , icon   = item |> Menu.icon
                , badge  = item |> badge
                }
              )
        in
          { name      = group
          , title     = group |> model.i18n.menu
          , badge     = entries |> List.filterMap .badge |> sum
          , collapsed = collapsed
          , items =
            if collapsed
              then entries |> List.filter .active
              else entries
          }
      )


sum : List Int -> Maybe Int
sum list =
  if list |> List.isEmpty
    then Nothing
    else list |> List.sum |> Just

match : String -> Menu.Item -> Bool
match path = Menu.href >> Href.path >> (==) path
