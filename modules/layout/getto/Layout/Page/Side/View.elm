module GettoCodes.Layout.Page.Side.View exposing
  ( Response
  , Breadcrumb
  , Menu
  , MenuI18n
  , response
  , menu
  , breadcrumb
  )
import GettoCodes.View.Href as Href exposing ( Href )
import GettoCodes.View.Menu as Menu
import GettoCodes.View.Icon as IconView exposing ( Icon )
import GettoCodes.View.Http as HttpView

import Getto.Http.Header.Decode as HeaderDecode

import Json.Decode as Decode

import Dict exposing ( Dict )

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader = ()
type alias ResponseBody =
  { badge : Dict String Int
  }

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

type alias MenuI18n =
  { menu  : String -> String
  , title : Href.Path -> String
  }


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.succeed ()
  , body = Decode.map ResponseBody
    ( Decode.list
      ( Decode.map2 Tuple.pair
        ( Decode.at ["name"]  Decode.string )
        ( Decode.at ["count"] Decode.int )
      )
    |> Decode.map Dict.fromList
    )
  }


type alias BreadcrumbModel =
  { path : Href.Path
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
  { path       : Href.Path
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

match : Href.Path -> Menu.Item -> Bool
match path = Menu.href >> Href.path >> (==) path
