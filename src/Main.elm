module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Route exposing (Route(..), parseUrl)
import Tree exposing (TreeNode, decodeTreeString, toString)
import Url
import Metadata exposing (Metadata(..), toPair)
import VisualTree


-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : State
    }


type State
    = Home
    | Viz { title : String, nodes : List TreeNode, activeMetadata : Maybe (List Metadata) }



-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { key = key
            , url = url
            , state = Home
            }
    in
    loadFromUrl url model



-- ROUTING CENTRAL LOGIC


loadFromUrl : Url.Url -> Model -> ( Model, Cmd Msg )
loadFromUrl url model =
    case parseUrl url of
        Tree name ->
            ( model, getTreeData name )

        _ ->
            ( { model | state = Home }, Cmd.none )



-- MESSAGES


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoadData String
    | GotTree (Result Http.Error String)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            loadFromUrl url { model | url = url }

        LoadData s ->
            ( model, getTreeData s )

        GotTree result ->
            case result of
                Ok data ->
                    let
                        nodes =
                            decodeTreeString data
                    in
                    case nodes of
                        Ok nodeList ->
                            ( { model
                                | state =
                                    Viz
                                        { title = "Visualization"
                                        , nodes = nodeList
                                        }
                              }
                            , Cmd.none
                            )
                        Err _ ->
                            ( { model | state = Home }, Cmd.none )

                Err _ ->
                    ( { model | state = Home }, Cmd.none )



-- HTTP


-- Muss für Verwendung mit Backend etwas umgewandelt werden
getTreeData : String -> Cmd Msg
getTreeData name =
    Http.get
        { url = "/data/" ++ name ++ ".json"
        , expect = Http.expectString GotTree
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "TaxoView"
    , body =
        [ headerView
        , contentView model.state
        , footerView
        ]
    }


headerView : Html Msg
headerView =
    div []
        [ a [ href "/" ] [ text "Back to Home" ]
        ]


footerView : Html Msg
footerView =
    div [] [ text "lfb241 & cactusiusss" ]


contentView : State -> Html Msg
contentView state =
    case state of
        Home ->
            div []
                [ text "Home: "
                , button [ onClick (LoadData "primates") ] [ text "Load sample (primates)" ]
                , button [ onClick (LoadData "felidae") ] [ text "Load sample (felidae)" ]

                ]

        {-| Viz viz ->
            div []
                [ text viz.title
                , div [] [ text (toString viz.nodes) ]
                ]
-}
        Viz viz ->
            div []
                [ h2 [] [ text viz.title ]
                , --here SVG-tree
                  div []
                    [ h3 [] [ text "Taxonomie-Baum" ]
                    , VisualTree.draw (Just viz.nodes)
                    ]
                , --show metadata
                  div []
                    [ h3 [] [ text "Metadaten Details" ]
                    , case viz.activeMetadata of
                        Nothing ->
                            p [] [ text "Klicken Sie auf einen Knoten, um Details zu sehen." ]

                        Just listOkMeta ->
                            ul [] 
                                (List.map (\meta -> 
                                    let 
                                        (title, value) = Metadata.toPair meta
                                    in 
                                    li [] [ b [] [ text (title ++ ": ") ], text value ]
                                 ) listOkMeta)
                    ]
                ]
