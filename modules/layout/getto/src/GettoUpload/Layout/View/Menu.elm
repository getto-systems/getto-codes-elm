module GettoUpload.Layout.View.Menu exposing
  ( Side
  , Breadcrumb
  , side
  , breadcrumb
  )
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
  , href  : String
  , icon  : Fa.Icon
  }

side : Model.Static -> Model.Layout -> Model.Plugin storage query -> List Side
side static layout plugin = []

breadcrumb : { a | path : String } -> MenuModel.Menu -> Maybe Breadcrumb
breadcrumb static =
  let
    find parent =
      List.foldl
        (\(MenuModel.Item icon href items) acc ->
          let
            push  stack = (icon,href) :: stack
            found stack = ( True,  stack |> push )
            into  stack = ( False, stack |> push )
          in
            case acc of
              ( True, _ ) -> acc
              ( False, stack ) ->
                if href == static.path
                  then stack |> found
                  else
                    case items |> find (stack |> into) of
                      ( True,  result ) -> ( True,  result )
                      ( False, result ) -> ( False, result |> List.drop 1 )
        )
        parent
  in
    List.filterMap
      (\(header,items) ->
        items
        |> find (False,[])
        |> toBreadcrumb header
      )
    >> List.head

toBreadcrumb : String -> ( Bool, List ( Fa.Icon, String ) ) -> Maybe Breadcrumb
toBreadcrumb header result =
  case result of
    (True,entries) ->
      Just
        ( header
        , entries
          |> List.reverse
          |> List.map
            (\(icon,href) ->
              { title = href |> I18n.title
              , href  = href
              , icon  = icon
              }
            )
        )
    _ -> Nothing
