module Main exposing (main)


import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Http
import Route exposing(Route(..),parseUrl)
import TypedSvg.Core
import Json.Decode


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
  , showTree : Maybe ( List(TreeNode))
  , showMetaData : Maybe ( List (MetaData))
  }



init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url Nothing Nothing, Cmd.none )

-- HTTP

getTreeData : String -> Cmd Msg
getTreeData url =
    Http.get
        { url = url
        , expect = Http.expectString GotTreeData
        }



-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotTreeData (Result Http.Error String)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            case parseUrl url of
                Tree treeName ->
                    ( { model
                        | url = url
                      }
                    , getTreeData treeName
                    )

                Node _ nodeName ->
                    ( { model
                        | url = url, showMetaData = (getMetaData nodeName)
                      }
                    , Cmd.none
                    )

                Home ->
                    ( { model
                        | url = url
                      }
                    , Cmd.none
                    )

        GotTreeData result ->
            case result of
                Ok fullData ->
                    let
                        tree =
                            decodeTreeString fullData
                    in
                    case tree of
                        Ok treeData ->
                            ( { model | showTree = Just treeData}, Cmd.none )

                        Err _ ->
                            (  {model | showTree = Nothing} , Cmd.none )

                Err _ ->
                    ( model, Cmd.none )





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



{-
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
        ) -}