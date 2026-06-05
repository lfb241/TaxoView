module Main exposing (main)


import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Json.Decode as D
import VirtualDom
import TypedSvg.Core exposing (Attribute)
import TypedSvg.Events as Ev
import TypedSvg.Types exposing (Length, Paint(..), percent, px)



-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url, Cmd.none )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]

      ]
  }


--- Hilfsmethode für Arbeiten mit Mausposition

type alias MousePosition =
    { x : Int
    , y : Int
    }


offsetMousePosition : D.Decoder MousePosition
offsetMousePosition =
    D.map2 MousePosition
        (D.field "offsetX" D.int)
        (D.field "offsetY" D.int)


onMouseMove : (MousePosition -> msg) -> Attribute msg
onMouseMove mapMousePositionToMsg =
    Ev.on "mousemove"
        (VirtualDom.Normal
            (D.map mapMousePositionToMsg offsetMousePosition)
        )