module GettoUpload.App.Upload.List.Search.Html exposing
  ( search
  , paging
  , table
  )
import GettoUpload.App.Upload.List.Search.View as View
import GettoUpload.View.Html as Html
import GettoUpload.View.Html.Button as ButtonHtml
import GettoUpload.View.Html.Field as FieldHtml
import GettoUpload.View.Html.Http as HttpHtml
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E


type alias SearchModel msg =
  { form  : View.View
  , http  : HttpView.Model View.ResponseHeader View.ResponseBody
  , options :
    { gender : List ( String, String )
    , roles  : List ( String, String )
    }
  , msg :
    { search : msg
    , input  : Form.Prop View.Form String -> String -> msg
    , check  : Form.Prop View.Form (Set String) -> String -> msg
    , change : msg
    }
  , i18n :
    { field : String -> String
    , form  : String -> String
    , http  : HttpView.Error -> String
    }
  }

search : SearchModel msg -> Html msg
search model =
  H.form [ model.msg.search |> E.onSubmit ]
    [ H.section []
      [ H.div []
        [ H.table []
          [ H.tbody [] <| List.concat
            [ case model.form |> View.name of
              (name,present,form) ->
                [ H.tr ( present |> FieldHtml.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> FieldHtml.text [] (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.form |> View.email of
              (name,present,form) ->
                [ H.tr ( present |> FieldHtml.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> FieldHtml.text [] (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.form |> View.tel of
              (name,present,form) ->
                [ H.tr ( present |> FieldHtml.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> FieldHtml.tel [] (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            ]
          ]
        ]
      , H.div []
        [ H.table []
          [ H.tbody [] <| List.concat
            [ case model.form |> View.age of
              (name,present,form) ->
                [ H.tr ( present |> FieldHtml.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.gteq.field |> FieldHtml.number [] (model.msg.input form.gteq.prop) model.msg.change
                    , " ～ " |> H.text
                    , form.lteq.field |> FieldHtml.number [] (model.msg.input form.lteq.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.form |> View.birthday of
              (name,present,form) ->
                [ H.tr ( present |> FieldHtml.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.gteq.field |> FieldHtml.date [] (model.msg.input form.gteq.prop) model.msg.change
                    , " ～ " |> H.text
                    , form.lteq.field |> FieldHtml.date [] (model.msg.input form.lteq.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.form |> View.start_at of
              (name,present,form) ->
                [ H.tr ( present |> FieldHtml.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.gteq.field |> FieldHtml.time [] (model.msg.input form.gteq.prop) model.msg.change
                    , " ～ " |> H.text
                    , form.lteq.field |> FieldHtml.time [] (model.msg.input form.lteq.prop) model.msg.change
                    ]
                  ]
                ]
            ]
          ]
        ]
      , H.div []
        [ H.table []
          [ H.tbody [] <| List.concat
            [ case model.form |> View.gender of
              (name,present,form) ->
                [ H.tr ( present |> FieldHtml.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> FieldHtml.select model.options.gender [] (model.msg.input form.prop) model.msg.change
                    ]
                  ]
                ]
            , case model.form |> View.roles of
              (name,present,form) ->
                [ H.tr ( present |> FieldHtml.isPresent )
                  [ H.th [] [ name |> model.i18n.field |> H.text ]
                  , H.td []
                    [ form.field |> FieldHtml.checkbox model.options.roles [] (model.msg.check form.prop) model.msg.change
                    ]
                  ]
                ]
            ]
          ]
        ]
      ]
    , H.footer [] <|
      case model.http |> HttpView.state of
        HttpView.Connecting progress ->
          [ "searching" |> model.i18n.form |> ButtonHtml.connecting
          , progress |> HttpHtml.progress
          ]
        HttpView.Ready error ->
          [ "search" |> model.i18n.form |> ButtonHtml.search
          , error |> HttpHtml.error model.i18n.http
          ]
    ]


type alias PagingModel msg =
  { page : Int
  , http : HttpView.Model View.ResponseHeader View.ResponseBody
  , msg :
    { page : String -> msg
    }
  , i18n :
    { paging : FieldHtml.Paging -> String
    }
  }

paging : PagingModel msg -> Html msg
paging model =
  case model.http |> HttpView.response of
    Nothing -> "" |> H.text
    Just res ->
      let
        header = res |> HttpView.header
        body   = res |> HttpView.body
      in
        if body |> List.isEmpty
          then "" |> H.text
          else { page = model.page, max = header.max } |> FieldHtml.paging model.i18n.paging model.msg.page


type alias TableModel =
  { http : HttpView.Model View.ResponseHeader View.ResponseBody
  , i18n :
    { field : String -> String
    }
  }

type Row
  = Upload View.Upload
  | Role Row String
  --| Comment Row View.Comment
  --| CommentRole Row String

table : TableModel -> Html msg
table model = "" |> H.text
            {-
  case model.http |> HttpView.response of
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
      in
        body |> List.map Upload |> Table.table
          (\upload -> [])
          [ Table.cell ( Table.None, Table.Double )
            (Table.th [] [ "id" |> model.i18n.field |> H.text ])
            (Table.th [] [ "sum" |> model.i18n.field |> H.text ])
            (\(Upload upload) ->
              Table.td [] [ upload.id |> String.fromInt |> H.text ]
            )
          , Table.group
            (Table.th [] [ "info" |> model.i18n.field |> H.text ])
            [ Table.cell ( Table.Single, Table.None )
              (Table.th [] [ "name" |> model.i18n.field |> H.text ])
              (Table.td [] [ sum |> String.fromInt |> H.text ])
              (\(Upload upload) ->
                Table.td [] [ upload.name |> H.text ]
              )
            , Table.cell ( Table.None, Table.None )
              (Table.th [] [ "gender" |> model.i18n.field |> H.text ])
              (Table.none)
              (\(Upload upload) ->
                Table.td [] [ upload.gender |> H.text ]
              )
            , Table.union roleLength ( Table.None, Table.None )
              (Table.th [] [ "roles" |> model.i18n.field |> H.text ])
              (Table.none)
              (\(Upload upload) -> upload.roles |> List.map (Upload upload |> Role))
              (\(Role (Upload upload) role) ->
                Table.td [] [ role |> H.text ]
              )
            , Table.rows
              (\(Upload upload) -> upload.roles |> List.map (Upload upload |> Role))
              [ Table.cell ( Table.None, Table.None )
                (Table.th [] [ "roles" |> model.i18n.field |> H.text ])
                (Table.none)
                (\(Role (Upload upload) role) ->
                  Table.td [] [ role |> H.text ]
                )
              ]
            , Table.parts genders
              (\gender ->
                [ Table.cell ( Table.None, Table.None )
                  (Table.th [] [ gender.value |> H.text ])
                  (Table.td [] [ gender.sum |> String.fromInt |> H.text ])
                  (\(Upload upload) ->
                    Table.td []
                      [ if upload.gender == gender.value
                        then "matched" |> H.text
                        else "" |> H.text
                      ]
                  )
                ]
              )
            , Table.rows
              (\(Upload upload) -> upload.comments |> List.map (Upload upload |> Comment))
              [ Table.cell ( Table.None, Table.None )
                (Table.th [] [ "comment" |> model.i18n.field |> H.text ])
                (Table.none)
                (\(Comment (Upload upload) comment) ->
                  Table.td [] [ comment.memo |> H.text ]
                )
              , Table.rows
                (\(Comment upload comment) -> comment.roles |> List.map (Comment upload comment |> CommentRole))
                [ Table.cell ( Table.None, Table.None )
                  (Table.th [] [ "roles" |> model.i18n.field |> H.text ])
                  (Table.none)
                  (\(CommentRole (Comment (Upload upload) comment) role) ->
                    Table.td [] [ role |> H.text ]
                  )
                ]
              ]
            ]
          ]
            -}
