module GettoUpload.App.Index.Page exposing
  ( init
  , subscriptions
  , onUrlRequest
  , onUrlChange
  , update
  , view
  )
import GettoUpload.Layout.Flags as Flags
import GettoUpload.Layout.Version as Version

import Browser
import Browser.Navigation as Navigation
import Url exposing ( Url )
import Html exposing ( Html )

type alias Model = ()

type Msg
  = UrlRequest Browser.UrlRequest
  | UrlChange Url

init : Flags.Model -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key = ( (), Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest = UrlRequest

onUrlChange : Url -> Msg
onUrlChange = UrlChange

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlRequest url -> ( model, Cmd.none )
    UrlChange urlRequest -> ( model, Cmd.none )

view : Model -> Browser.Document Msg
view model =
  { title = "Title"
  , body =
    [ Version.copyright ++ Version.version
      |> Html.text
    ]
  }
