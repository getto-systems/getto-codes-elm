module GettoUpload.App.Data.Upload.List.Search.Html exposing
  ( search
  , paging
  , table
  )
import GettoUpload.App.Data.Upload.List.Search.View as View
import GettoUpload.Extension.Href as Href
import GettoUpload.Extension.Href.Data.Upload as Upload
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
import Getto.Field.Present as Present
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
    , role   : List ( String, String )
    }
  , msg :
    { request : msg
    , input   : View.Prop String -> String -> msg
    , toggle  : View.Prop (Set String) -> String -> msg
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
                    [ form.field |> Input.text [] (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.view.email of
              (name,form,opts) ->
                [ H.tr ( opts.isPresent |> Input.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> Input.text [] (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.view.tel of
              (name,form,opts) ->
                [ H.tr ( opts.isPresent |> Input.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> Input.tel [] (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            ]
          ]
        ]
      , H.div []
        [ H.table []
          [ H.tbody [] <| List.concat
            [ case model.view.age of
              (name,form,opts) ->
                [ H.tr ( opts.isPresent |> Input.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.gteq.field |> Input.number [] (model.msg.input form.gteq.prop) model.msg.change
                    , " ～ " |> H.text
                    , form.lteq.field |> Input.number [] (model.msg.input form.lteq.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.view.birthday of
              (name,form,opts) ->
                [ H.tr ( opts.isPresent |> Input.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.gteq.field |> Input.date [] (model.msg.input form.gteq.prop) model.msg.change
                    , " ～ " |> H.text
                    , form.lteq.field |> Input.date [] (model.msg.input form.lteq.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.view.start_at of
              (name,form,opts) ->
                [ H.tr ( opts.isPresent |> Input.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.gteq.field |> Input.time [] (model.msg.input form.gteq.prop) model.msg.change
                    , " ～ " |> H.text
                    , form.lteq.field |> Input.time [] (model.msg.input form.lteq.prop) model.msg.change
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
                    [ form.field |> Input.select model.options.gender [] (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.view.roles of
              (name,form,opts) ->
                [ H.tr ( opts.isPresent |> Input.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> Input.checkbox model.options.role [] (model.msg.toggle form.prop) model.msg.change
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
      , [ " " |> H.text
        , H.a [ Upload.new |> Href.toString |> A.href ]
          [ Icon.edit |> Html.icon []
          , " " |> H.text
          , "new" |> model.i18n.form |> H.text
          ]
        ]
      ]
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
  case model.get |> HttpView.response of
    Nothing -> "" |> H.text
    Just res ->
      let
        header = res |> HttpView.header
        body   = res |> HttpView.body
      in
        if body |> List.isEmpty
          then "" |> H.text
          else { page = model.page, max = header.max } |> Input.paging model.i18n.paging model.msg.page


type alias TableModel msg =
  { get  : HttpView.Model View.Response
  , sort : Sort.Value
  , msg :
    { sort : Sort.Value -> msg
    }
  , i18n :
    { field : String -> String
    , table : String -> String
    , form  : String -> String
    }
  }

table : TableModel msg -> Html msg
table model =
  case model.get |> HttpView.response of
    Nothing -> "" |> H.text
    Just res ->
      let
        body = res |> HttpView.body
        sum = 18
        roleLength = 2
        genders =
          [ { value = "male"
            , sum = 2
            }
          , { value = "female"
            , sum = 3
            }
          , { value = "other"
            , sum = 1
            }
          ]

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
              [ H.p []
                [ H.a [ upload.id |> Upload.edit |> Href.toString |> A.href ]
                  [ Icon.edit |> Html.icon []
                  , " " |> H.text
                  , "detail" |> model.i18n.form |> H.text
                  ]
                ]
              ]
            }
          , Table.group ( Table.th [ "is-center" |> A.class ] [ "info" |> model.i18n.field |> H.text ] )
            [ Table.column ( Table.none, Table.none )
              { header  = Table.th [] [ "name" |> model.i18n.field |> H.text ]
              , summary = Table.th [] [ "sum" |> model.i18n.field |> H.text ]
              , content = \upload -> Table.td []
                [ H.p [] [ upload.name |> H.text ] ]
              }
            , Table.column ( Table.none, Table.single )
              { header  = Table.th [] [ "gender" |> model.i18n.field |> H.text ]
              , summary = Table.td [] [ H.p [] [ sum |> String.fromInt |> H.text ] ]
              , content = \upload -> Table.td []
                [ H.p [] [ upload.gender |> H.text ] ]
              }
            , Table.union ( Table.none, Table.none )
              { header  = Table.th [] [ "roles" |> model.i18n.field |> H.text ]
              , summary = Table.empty
              , colspan = roleLength
              , data    = \upload -> upload.roles |> List.map (\role -> ( upload, role ))
              , content = \(upload,role) -> Table.td []
                [ H.p [] [ role |> H.text ] ]
              }
            , Table.parts genders
              (\gender ->
                [ Table.column ( Table.none, Table.none )
                  { header  = Table.th [] [ gender.value |> H.text ]
                  , summary = Table.td [ "is-center" |> A.class ]
                    [ H.p [] [ gender.sum |> String.fromInt |> H.text ] ]
                  , content = \upload -> Table.td [ "is-center" |> A.class ]
                    [ H.p []
                      [ if upload.gender == gender.value
                        then Icon.far "check-circle" |> Html.icon []
                        else "" |> H.text
                      ]
                    ]
                  }
                , Table.column ( Table.none, Table.none )
                  { header  = Table.th [] [ gender.value |> H.text ]
                  , summary = Table.td [ "is-center" |> A.class ]
                    [ H.p [] [ gender.sum |> String.fromInt |> H.text ] ]
                  , content = \upload -> Table.td [ "is-center" |> A.class ]
                    [ H.p []
                      [ if upload.gender /= gender.value
                        then Icon.fas "times" |> Html.icon []
                        else "" |> H.text
                      ]
                    ]
                  }
                ]
              )
            , Table.rows ( \upload -> upload.roles |> List.map (\role -> ( upload, role )) )
              [ Table.column ( Table.none, Table.none )
                { header  = Table.th [] [ "roles" |> model.i18n.field |> H.text ]
                , summary = Table.empty
                , content = \(upload,role) -> Table.td []
                  [ H.p [] [ role |> H.text ] ]
                }
              , Table.column ( Table.none, Table.none )
                { header  = Table.th [] [ "roles" |> model.i18n.field |> H.text ]
                , summary = Table.empty
                , content = \(upload,role) -> Table.td []
                  [ H.p [] [ role |> H.text ] ]
                }
              ]
            , Table.group ( Table.th [ "is-center" |> A.class ] [ "comment" |> model.i18n.field |> H.text ] )
              [ Table.rows ( \upload -> upload.comments |> List.map (\comment -> ( upload, comment )) )
                [ Table.column ( Table.single, Table.none )
                  { header  = Table.th [] [ "user" |> model.i18n.field |> H.text ]
                  , summary = Table.empty
                  , content = \(upload,comment) -> Table.td []
                    [ H.p [] [ comment.user |> H.text ] ]
                  }
                , Table.column ( Table.none, Table.none )
                  { header  = Table.th [] [ "text" |> model.i18n.field |> H.text ]
                  , summary = Table.empty
                  , content = \(upload,comment) -> Table.td []
                    [ H.p [] [ comment.text |> H.text ] ]
                  }
                , Table.group ( Table.th [ "is-center" |> A.class ] [ "like" |> model.i18n.field |> H.text ] )
                  [ Table.rows
                    ( \(upload,comment) -> comment.likes |> List.map (\like -> ( upload, comment, like )) )
                    [ Table.column ( Table.single, Table.none )
                      { header  = Table.th [] [ "user" |> model.i18n.field |> H.text ]
                      , summary = Table.empty
                      , content = \(upload,comment,like) -> Table.td []
                        [ H.p [] [ like.user |> H.text ] ]
                      }
                    , Table.column ( Table.none, Table.none )
                      { header  = Table.th [] [ "text" |> model.i18n.field |> H.text ]
                      , summary = Table.empty
                      , content = \(upload,comment,like) -> Table.td []
                        [ H.p [] [ like.text |> H.text ] ]
                      }
                    ]
                  ]
                ]
              ]
            ]
          ]
