module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, h2, h3, li, p, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Route exposing (Route(..), parseUrl)
import Tree exposing (Metadata(..),metadataToList,TreeNode(..), decodeTreeString)
import Url
import VisualTree
import Html.Attributes exposing (class)
import Html exposing (footer)
import Html exposing (section)
import Html.Attributes exposing (id)

{-
TODO: 
- Layout definieren
- headerView
- footerView
- contentView
- metadataView als "Card" über pressed Note
- Suchfeld implementieren 

DONE:
- LoadData durch korrektes LinkClicked ersetzen, sodass auch URL sich ändert (bei Tree und Nodeclicked)
-}

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

-- State beschreibt aktuelle Page
type State
    = Home
    | Viz
        { title : String
        , treename: String
        , nodes : List TreeNode
        , activeMetadata : Maybe (List Metadata)
        }

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : State
    }

-- Initialisierung mit Homepage
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

-- Funktion um je nach Url die richtige Seite
-- mit den richtigen HTTP-Requests zu laden
loadFromUrl : Url.Url -> Model -> ( Model, Cmd Msg )
loadFromUrl url model =
    case parseUrl url of
        Tree name ->
            ( model, getTreeData name )

        Node treename nodename ->
            case model.state of
                Viz viz ->
                    let
                        metadata =
                            viz.nodes
                                |> findNode nodename
                                |> Maybe.andThen (\(TreeNode _ _ _ meta _) -> meta)
                    in
                    ( { model | state = Viz { viz | activeMetadata = metadata } }
                    , Cmd.none
                    )
                {- Geht nicht weil SPA
                wenn eine node-url geladen wird ohne dass wir vorher einen Baum geladen haben
                -- soll erstmal nur der korrespondierende Baum geladen werden  
                _ ->
                    ( model, getTreeData treename )-}
                _ -> (model, Cmd.none)
        _ ->
            ( { model | state = Home }, Cmd.none )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
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


        GotTree result ->
            case result of
                Ok data ->
                    case decodeTreeString data of

                        Ok nodeList ->
                            case parseUrl model.url of
                                -- wenn wir von einer tree anfrage kommen
                                Tree name ->
                                    ( { model
                                    | state =
                                        Viz
                                            { title = "Visualization"
                                            , treename = name
                                            , nodes = nodeList
                                            , activeMetadata = Nothing
                                            }
                                        }
                                        , Cmd.none
                                    )
                                {- IRRELEVANT, da das bei unserer SPA eh nicht geht 
                                wenn wir eine node anfrage tätigen, obwohl tree noch nicht geladen
                                Node name _ ->
                                    ({model
                                    | state =
                                        Viz
                                            { title = "Visualization"
                                            , treename = name
                                            , nodes = nodeList
                                            , activeMetadata = Nothing
                                            }
                                        }
                                        --- url korrekt setzen
                                        , Nav.pushUrl model.key ("/TaxoView/tree/" ++ name)
                                    ) -}
                                _ ->
                                    (model, Cmd.none)

                        Err _ ->
                            ( { model | state = Home }, Cmd.none )

                Err _ ->
                    ( { model | state = Home }, Cmd.none )


        SelectNode node ->
          case node of
                TreeNode id _ _ _ _ ->
                    case model.state of 
                        Viz viz ->

                            ( model
                            , Nav.pushUrl model.key ("/TaxoView/tree/" ++ viz.treename ++ "/node/" ++ id)
                            )
                        _ ->
                            (model, Cmd.none)


-- Je nach Backend muss diese Funktion angepasst werden
-- in unserem Fall laden wir Beispieldaten die auf dem Frontend-Server liegen
getTreeData : String -> Cmd Msg
getTreeData name =
    Http.get
        { url = "/data/" ++ name ++ ".json" 
        -- TODO: ändern für productive
        --url = "/docs/data" ++ name ++ ".json"
        , expect = Http.expectString GotTree
        }

-- Hier wird alles zusammengebaut
view : Model -> Browser.Document Msg
view model =
    { title = "TaxoView"
    , body =
        [ div[][headerView
        , section [class "section content-section"] [div [class "container"][contentView model.state]]
        , footerView
        ]]
    }


headerView : Html Msg
headerView =
    div []
        [ section [ class "hero is-link" ]
            [ div [ class "hero-body py-4" ]
                [ div [ class "container" ]
                    [ p [ class "title" ] [ text "TaxoView" ]
                    , p [ class "subtitle" ]
                        [ text "Visualisierung phylogenetischer Stammbäume" ]
                    ]
                ]
            ]]

footerView : Html Msg
footerView =
    footer [ class "footer" ]
        [ div [class "container"]
            [ text "Projekt für das WWW-Modul SoSe26, von Luke-Felix Brüske und Katherina Shapilova"
            ]
        , div [class "container"]
            [ a [ href "https://github.com/lfb241/TaxoView" ]
                [ text "Github-Code" ]
            ]
        ]

-- dynamische view-Funktion rendert je nach State den Inhalt
contentView : State -> Html Msg
contentView state =
    case state of
        Home ->
            div [class "buttons"]
                [ a [href "/TaxoView/"] [text "Home: "]
                , a [ href "/TaxoView/tree/primates", class "button is-info" ] [ text "Load sample (primates)" ]
                , a [ href "/TaxoView/tree/felidae", class "button is-info" ] [ text "Load sample (felidae)" ]
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

-- Hilfs-View-Funktion zur Anzeige von Metadaten
viewMetadata : Maybe (List Metadata) -> Html Msg
viewMetadata maybeMetadata =
    case maybeMetadata of
        Nothing ->
            p [] [ text "Klicken Sie auf einen Knoten, um Details zu sehen." ]

        Just metadataList ->
            text "testtestetst"

-- rekursive Methode um im Baum den Knoten mit der richtigen ID zu finden
findNode : String -> List TreeNode -> Maybe TreeNode
findNode targetId nodes =
    -- Abbruchbedingung
    case nodes of
        [] ->
            Nothing
        -- Dekonstruierung der Liste
        (TreeNode id label rank meta children) :: rest ->
            if id == targetId then
                -- Wenn Kopf der Liste der gesuchte Knoten ist
                Just (TreeNode id label rank meta children)
            else
                -- weitersuchen in Children-Knoten
                case Maybe.andThen (findNode targetId) children of
                    Just found ->
                        Just found
                    Nothing ->
                        findNode targetId rest