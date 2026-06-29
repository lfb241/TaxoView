module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, h2, h3, li, p, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Metadata exposing (Metadata, toPair)
import Route exposing (Route(..), parseUrl)
import Tree exposing (TreeNode(..), decodeTreeString)
import Url
import VisualTree


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


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : State
    }


type State
    = Home
    | Viz
        { title : String
        , nodes : List TreeNode
        , activeMetadata : Maybe (List Metadata)
        }


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


loadFromUrl : Url.Url -> Model -> ( Model, Cmd Msg )
loadFromUrl url model =
    case parseUrl url of
        Tree name ->
            ( model, getTreeData name )

        _ ->
            ( { model | state = Home }, Cmd.none )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoadData String
    | GotTree (Result Http.Error String)
    | SelectNode TreeNode


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

        LoadData name ->
            ( model, getTreeData name )

        GotTree result ->
            case result of
                Ok data ->
                    case decodeTreeString data of
                        Ok nodeList ->
                            ( { model
                                | state =
                                    Viz
                                        { title = "Visualization"
                                        , nodes = nodeList
                                        , activeMetadata = Nothing
                                        }
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | state = Home }, Cmd.none )

                Err _ ->
                    ( { model | state = Home }, Cmd.none )

        SelectNode (TreeNode _ _ _ maybeMetadata _) ->
            case model.state of
                Viz viz ->
                    ( { model
                        | state =
                            Viz { viz | activeMetadata = maybeMetadata }
                      }
                    , Cmd.none
                    )

                Home ->
                    ( model, Cmd.none )


getTreeData : String -> Cmd Msg
getTreeData name =
    Http.get
        { url = "/TaxoView/data/" ++ name ++ ".json"
        , expect = Http.expectString GotTree
        }


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

        Viz viz ->
            div []
                [ h2 [] [ text viz.title ]
                , div []
                    [ h3 [] [ text "Taxonomie-Baum" ]
                    , VisualTree.draw SelectNode (Just viz.nodes)
                    ]
                , div []
                    [ h3 [] [ text "Metadaten Details" ]
                    , viewMetadata viz.activeMetadata
                    ]
                ]


viewMetadata : Maybe (List Metadata) -> Html Msg
viewMetadata maybeMetadata =
    case maybeMetadata of
        Nothing ->
            p [] [ text "Klicken Sie auf einen Knoten, um Details zu sehen." ]

        Just metadataList ->
            ul []
                (List.map
                    (\meta ->
                        let
                            ( title, value ) =
                                toPair meta
                        in
                        li [] [ b [] [ text (title ++ ": ") ], text value ]
                    )
                    metadataList
                )