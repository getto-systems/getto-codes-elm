module GettoUpload.App.Upload.ListEdit.Search.Html exposing
  ( search
  , pairs
  , paging
  , table
  )
import GettoUpload.App.Upload.ListEdit.Data.View   as Data
import GettoUpload.App.Upload.ListEdit.Search.View as View
import GettoUpload.Extension.Href as Href
import GettoUpload.Extension.Href.Upload as Upload
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as Button
import GettoUpload.View.Html.Input as Input
import GettoUpload.View.Html.Http as Http
import GettoUpload.View.Html.Table as TableView
import GettoUpload.View.Html.Sort as SortView
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Edit exposing ( State(..) )
import Getto.Field.Conflict as Conflict
import Getto.Sort as Sort
import Getto.Html.Table as Table

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias SearchModel msg =
  { view : View.View
  , get  : HttpView.Model View.Response
  , options :
    { gender : List ( String, String )
    }
  , msg :
    { request : msg
    , input   : View.Prop String -> String -> msg
    , change  : msg
    }
  , i18n :
    { field : String -> String
    , form  : String -> String
    , http  : HttpView.Error -> String
    }
  }

search : SearchModel msg -> Html msg
search model =
  H.form [ model.msg.request |> E.onSubmit ]
    [ H.section []
      [ H.div []
        [ H.table []
          [ H.tbody [] <| List.concat
            [ case model.view.name of
              (name,form,present) ->
                [ H.tr ( present |> Input.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> Input.text []
                      (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            ]
          ]
        ]
      , H.div []
        [ H.table []
          [ H.tbody [] <| List.concat
            [ case model.view.gender of
              (name,form,present) ->
                [ H.tr ( present |> Input.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> Input.select model.options.gender []
                      (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            ]
          ]
        ]
      ]
    , H.footer [] <| List.concat
      [ case model.get |> HttpView.state of
        HttpView.Connecting progress ->
          [ "searching" |> model.i18n.form |> Button.connecting
          , progress |> Http.progress
          ]
        HttpView.Ready error ->
          [ "search" |> model.i18n.form |> Button.search
          , error |> Http.error model.i18n.http
          ]
      ]
    ]

pairs : View.Form -> List ( String, String )
pairs form =
  [ form.name |> Field.pair
  ]


type alias PagingModel msg =
  { page : Int
  , get  : HttpView.Model View.Response
  , msg :
    { page : String -> msg
    }
  , i18n :
    { paging : Input.Paging -> String
    }
  }

paging : PagingModel msg -> Html msg
paging model =
  case model.get  |> HttpView.response of
    Nothing -> "" |> H.text
    Just res ->
      let
        header = res |> HttpView.header
        body   = res |> HttpView.body
      in
        if body |> List.isEmpty
          then "" |> H.text
          else { page = model.page, max = header.max } |> Input.paging model.i18n.paging model.msg.page


type alias TableModel msg info =
  { get  : HttpView.Model View.Response
  , sort : Sort.Model
  , info : Table.Column Data.Upload info
  , msg :
    { sort : Sort.Model -> msg
    , info : info -> msg
    }
  , i18n :
    { field : String -> String
    , table : String -> String
    , form  : String -> String
    }
  }

table : TableModel msg info -> Html msg
table model =
  case model.get |> HttpView.response of
    Nothing -> "" |> H.text
    Just res ->
      let
        body = res |> HttpView.body

        sort = SortView.render
          { current = model.sort
          , msg     = model.msg.sort
          }
      in
        body |> Table.render (TableView.config model.i18n.table)
          [ Table.column ( Table.none, Table.none )
            { header  = Table.th [] ( [ "id" |> model.i18n.field |> H.text ] |> sort "id" )
            , summary = Table.empty
            , content = \upload -> Table.td [ "is-center" |> A.class ]
              [ H.p [] [ upload.id |> String.fromInt |> H.text ] ]
            }
          , Table.column ( Table.none, Table.double )
            { header  = Table.th [] []
            , summary = Table.empty
            , content = \upload -> Table.td []
              [ H.p []
                [ H.a [ upload.id |> Upload.edit |> Href.toString |> A.href ]
                  [ Icon.edit |> Html.icon []
                  , " " |> H.text
                  , "detail" |> model.i18n.form |> H.text
                  ]
                ]
              ]
            }
          , model.info |> Table.map model.msg.info
          ]
