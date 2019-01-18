module GettoUpload.App.Index.Page exposing ( main )
import GettoUpload.Layout.Flags as Flags
import GettoUpload.Layout.Version as Version

import Browser
import Browser.Navigation as Navigation
import Url exposing ( Url )
import Html as H exposing ( Html )
import Html.Attributes as A

main = Browser.application
  { init          = init
  , subscriptions = subscriptions
  , onUrlRequest  = UrlRequest
  , onUrlChange   = UrlChange
  , update        = update
  , view          = view
  }

type alias Model =
  { key : Navigation.Key
  }

type Msg
  = UrlRequest Browser.UrlRequest
  | UrlChange Url

init : Flags.Model -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key = ( { key = key }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->  ( model, url  |> Url.toString |> Navigation.load )
        Browser.External href -> ( model, href |> Navigation.load )
    UrlChange url -> ( model, Cmd.none )

view : Model -> Browser.Document Msg
view model =
  { title = "Title"
  , body =
    [ H.p [] [ H.a [ A.href "/dist/index.html" ] [ "TOP" |> H.text ] ]
    , H.p [] [ H.a [ A.href "/dist/index.html#abc" ] [ "TOP" |> H.text ] ]
    , H.p [] [ H.a [ A.href "https://google.com" ] [ "GOOGLE" |> H.text ] ]
    , H.p [] [ Version.copyright ++ Version.version |> H.text ]
    ]
  }
