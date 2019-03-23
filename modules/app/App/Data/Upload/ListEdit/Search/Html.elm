module GettoCodes.App.Data.Upload.ListEdit.Search.Html exposing
  ( search
  , pairs
  , paging
  , table
  )
import GettoCodes.App.Data.Upload.ListEdit.Data.View   as Data
import GettoCodes.App.Data.Upload.ListEdit.Search.View as View
import GettoCodes.Extension.Href as Href
import GettoCodes.Extension.Href.Data.Upload as Upload
import GettoCodes.View.Html as Html
import GettoCodes.View.Html.Button as Button
import GettoCodes.View.Html.Input as Input
import GettoCodes.View.Html.Http as Http
import GettoCodes.View.Html.Table as TableView
import GettoCodes.View.Html.Sort as SortView
import GettoCodes.View.Icon as Icon
import GettoCodes.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
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
              (name,form,opts) ->
                [ H.tr ( opts.isPresent |> Input.isPresent )
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
              (name,form,opts) ->
                [ H.tr ( opts.isPresent |> Input.isPresent )
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
  [ form.name |> Field.id_value
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
  , sort : Sort.Value
  , info : Table.Column Data.Upload info
  , msg :
    { sort : Sort.Value -> msg
    , info : info -> msg
    , edit : Data.Upload -> msg
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
          { sort = model.sort
          , msg  = model.msg.sort
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
              [ "edit" |> model.i18n.form |> Button.edit (model.msg.edit upload)
              ]
            }
          , model.info |> Table.map model.msg.info
          ]
