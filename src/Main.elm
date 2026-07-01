module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, a, b, button, div, footer, form, h2, h3, input, li, p, section, strong, text, ul)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Route exposing (Route(..), parseUrl)
import Tree exposing (Metadata(..), TreeNode(..), decodeTreeString, metadataToList)
import Url
import VisualTree
import List
import Html exposing (table)
import Html exposing (thead)
import Html exposing (tr)
import Html exposing (th)
import Html exposing (tbody)
import Html exposing (td)
import Html exposing (span)
import Html exposing (i)
import Html.Attributes exposing (style)



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
        { keyValueData : Maybe (List (String, String))
        , formString : String
        , results: Maybe (List String)
        , showResultTable: Bool
        }
    | Viz
        { title : String
        , treename : String
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
            , state = Home { keyValueData = Nothing, formString = "", showResultTable=False, results = Nothing }
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

        Node _ nodename ->
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
                       ( model, getTreeData treename )
                -}
                _ ->
                    ( model, Cmd.none )


        Search query ->
            case model.state of
                Home _ ->
                    ({model | state = Home {keyValueData=Nothing, formString= Maybe.withDefault "" query, showResultTable=False, results=Nothing}} , Http.get
                        { url = "/TaxoView/data/name_pairs.json"
                        , expect = Http.expectJson (GotKeyValueData query) (Decode.keyValuePairs Decode.string)
                        })
                Viz _ ->
                    ({model | state = Home {keyValueData= Nothing, formString = Maybe.withDefault "" query, showResultTable= False, results=Nothing}}, Cmd.none)
        _ ->
            ( { model | state = Home { keyValueData = Nothing, formString = "", showResultTable=False, results = Nothing  } }, Cmd.none )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotTree (Result Http.Error String)
    | SelectNode TreeNode
    | UpdateFormString String
    | SubmitForm
    | GotKeyValueData (Maybe String) (Result Http.Error (List (String, String)))


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
                                       )
                                -}
                                _ ->
                                    ( model, Cmd.none )

                        Err _ ->
                            ( { model | state = Home { formString = "", keyValueData = Nothing, showResultTable=False, results=Nothing } }, Cmd.none )

                Err _ ->
                    ( { model | state = Home { formString = "", keyValueData = Nothing, showResultTable=False, results=Nothing } }, Cmd.none )

        SelectNode node ->
            case node of
                TreeNode id _ _ _ _ ->
                    case model.state of
                        Viz viz ->
                            ( model
                            , Nav.pushUrl model.key ("/TaxoView/tree/" ++ viz.treename ++ "/node/" ++ id)
                            )

                        _ ->
                            ( model, Cmd.none )

        UpdateFormString string ->
            case model.state of
                Home home ->
                    ( { model | state = Home { home | formString = string } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitForm ->
            case model.state of
                Home home ->
                    ( model
                    , Nav.pushUrl model.key ("/TaxoView/search?query=" ++ home.formString)
                    )
                _ ->
                    (model, Cmd.none)

        GotKeyValueData query result ->
            case model.state of
                Home home ->
                    case result of
                        Ok data ->
                            case findMatchingValues (Maybe.withDefault "" query) data of
                                results ->
                                    ( { model | state = Home {home | keyValueData = Just data, showResultTable=True, results=Just results}}, Cmd.none)
                        Err _ ->
                            (  model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

-- Je nach Backend muss diese Funktion angepasst werden
-- in unserem Fall laden wir Beispieldaten die auf dem Frontend-Server liegen


getTreeData : String -> Cmd Msg
getTreeData name =
    Http.get
        { url = "/TaxoView/data/" ++ name ++ ".json"

        -- TODO: ändern für productive
        --url = "/docs/data" ++ name ++ ".json"
        , expect = Http.expectString GotTree
        }



-- Hier wird alles zusammengebaut

view : Model -> Browser.Document Msg
view model =
    { title = "TaxoView"
    , body =
        [ div [ class "is-flex is-flex-direction-column", style "min-height" "100vh" ]            
        [ headerView
            , section [ class "section is-flex-grow-1" ]
                [ div [ class "container" ] [ contentView model ] ]
            , footerView
            ]
        ]
    }


headerView : Html Msg
headerView =
    section [ class "hero is-link" ]
        [ div [ class "hero-body py-5" ]
            [ div [ class "container" ]
                [ p [ class "title" ] [ text "TaxoView" ]
                , p [ class "subtitle" ]
                    [ text "Visualisierung phylogenetischer Stammbäume" ]
                ]
            ]
        ]


footerView : Html Msg
footerView =
    footer [ class "footer py-3" ]
        [ div [ class "content" ]
            [ p []
                [ text "Projekt für das WWW-Modul SoSe26, von Luke-Felix Brüske und Katherina Shapilova" ]
            , p []
                [ a [ href "https://github.com/lfb241/TaxoView", class "has-text-link" ]
                    [ text "Github-Code" ]
                ]
            ]
        ]


contentView : Model -> Html Msg
contentView model =
    case model.state of
        Home home ->
            div [ class "container" ]
                [ div [ class "box" ]
                    [ p [ class "mb-2" ]
                        [ text "Gib einen Suchbegriff ein, um biologische Organismen in der Datenbank zu finden." ]
                    , p []
                        [ text "Klicke anschließend auf 'Visualisierung', um den zugehörigen Baum zu öffnen." ]
                    ]
                , form [ onSubmit SubmitForm ]
                    [ div [ class "field has-addons is-justify-content-center" ]
                        [ div [ class "control has-icons-left is-expanded" ]
                            [ input
                                [ class "input is-link"
                                , type_ "text"
                                , placeholder "Suche in Datenbank nach biologischen Entitäten..."
                                , value home.formString
                                , onInput UpdateFormString
                                ]
                                []
                            , span [ class "icon is-left" ]
                                [ i [ class "fas fa-search" ] [] ]
                            ]
                        , div [ class "control" ]
                            [ button
                                [ class "button is-link", type_ "submit" ]
                                [ text "Suchen" ]
                            ]
                        ]
                    ]
                , div [ class "container mt-4" ]
                    (if home.showResultTable then
                        [ div [ class "table-container box" ]
                            [ table [ class "table is-striped is-hoverable is-fullwidth" ]
                                [ thead []
                                    [ tr []
                                        [ th [] [ text "Biologische Entitäten:" ]
                                        , th [] []
                                        ]
                                    ]
                                , tbody []
                                    (if List.isEmpty (Maybe.withDefault [] home.results) then
                                        [ tr []
                                            [ td [ class "has-text-centered" ]
                                                [ div [ class "notification is-warning is-light" ]
                                                    [ text "Keine Einträge gefunden... Versuche es nochmal!" ]
                                                ]
                                            , td [] []
                                            ]
                                        ]

                                     else
                                        List.map
                                            (\x ->
                                                tr []
                                                    [ td [ class "is-vcentered" ] [ text x ]
                                                    , td [ class "is-vcentered has-text-right" ]
                                                        [ a
                                                            [ href ("/TaxoView/tree/" ++ x)
                                                            , class "button is-info is-small"
                                                            ]
                                                            [ text "Visualisierung" ]
                                                        ]
                                                    ]
                                            )
                                            (Maybe.withDefault [] home.results)
                                    )
                                ]
                            ]
                        ]

                     else
                        []
                    )
                ]

        Viz viz ->
            div [ class "container" ]
                [ div [ class "box" ]
                    [ p [ class "title is-5" ]
                        [ text ("Visualisierung von: " ++ viz.treename) ]
                    , p [ class "subtitle is-6" ]
                        [ text "Klicke auf Knoten, um auf weitere Daten der Entität zuzugreifen..." ]
                    ]
                ,div [ class "box" ]
                    [ div [ class "is-flex is-justify-content-center" ]
                        [ div [ style "width" "100%", style "max-width" "1000px" ]
                        [ VisualTree.draw SelectNode (Just viz.nodes) ]
                        ]
                    ]
                , div [ class "box" ]
                    [ p [ class "title is-6" ] [ text "Metadaten" ]
                    , viewMetadata viz.activeMetadata
                    ]
                ]


viewMetadata : Maybe (List Metadata) -> Html Msg
viewMetadata maybeMetadata =
    case maybeMetadata of
        Nothing ->
            p [ class "has-text-grey" ] [ text "Klicken Sie auf einen Knoten, um Details zu sehen." ]

        Just metadataList ->
            div [ class "content" ]
                [ table [ class "table is-fullwidth is-narrow" ]
                    [ tbody []
                        (List.map
                            (\( key, val ) ->
                                tr []
                                    [ th [ class "has-text-link", style "width" "40%" ] [ text key ]
                                    , td [] [ text val ]
                                    ]
                            )
                            (metadataToList metadataList)
                        )
                    ]
                ]

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


findMatchingValues : String -> List ( String, String ) -> List String
findMatchingValues query pairs =
    case query of
        "" ->
            []
        _ -> 
            pairs
                |> List.filter (\( key, _ ) -> String.contains query key)
                |> List.map (\( _, value ) -> value)