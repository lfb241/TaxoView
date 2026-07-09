module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, footer, form, h2, h3, input, li, p, section, strong, text, ul)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import Route exposing (Route(..), parseUrl)
import TaxonTree exposing (Metadata(..), TreeNode(..), decodeTreeString, metadataToPairs)
import Url
import VisualTree.draw SelectNode (Just viz.nodes)
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

   - metadataView als "Card" über pressed Note

   DONE:
   - LoadData durch korrektes LinkClicked ersetzen, sodass auch URL sich ändert (bei Tree und Nodeclicked)
    - Layout definieren
   - headerView
   - footerView
   - contentView
    - Suchfeld implementieren

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



-- State beschreibt aktuelle Page mit Attributen
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



-- Funktion um onUrlChange je nach Url die richtige Seite
-- mit den richtigen HTTP-Requests zu laden
loadFromUrl : Url.Url -> Model -> ( Model, Cmd Msg )
loadFromUrl url model =
    -- parseUrl aus Route.elm gibt ein Objekt vom Typ Route aus
    case parseUrl url of
        
        Tree name ->
            ( model, getTreeData name )


        Node _ nodename ->
            case model.state of
                Viz viz ->
                    let
                        metadata =
                            viz.nodes
                                -- Suchen des TreeNodes mit findNode
                                |> findNode nodename
                                -- Extraktion der Metadaten
                                |> Maybe.andThen (\(TreeNode _ _ _ meta _) -> meta)
                    in
                    ( { model | state = Viz { viz | activeMetadata = metadata } }
                    , Cmd.none
                    )
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
    | UpdateFormString String
    | SubmitForm
    | GotKeyValueData (Maybe String) (Result Http.Error (List (String, String)))
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
                                -- wenn wir von einer tree-Anfrage kommen
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
            -- Pattern Matching ermöglicht Überschreiben einzelner Attribute von Home 
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
                            -- findMatchingValues sucht Substrings in Keys und gibt Value-List aus 
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
        , expect = Http.expectString GotTree
        }


view : Model -> Browser.Document Msg
view model =
    { title = "TaxoView"
    , body =
        
        -- CSS-Flexbox mit einer Spalte, immer mindestens den 100% der ViewPort-Höhe hoch

        [ div [ class "is-flex is-flex-direction-column", style "min-height" "100vh" ]            
            
            -- Header mit Bulma-Hero --
            [ section [ class "hero is-link" ]
                [ div [ class "hero-body py-5" ]
                    [ div [ class "container" ]
                        [ p [ class "title" ] [ text "TaxoView" ]
                            , p [ class "subtitle" ] [ text "Visualisierung phylogenetischer Stammbäume" ]
                        ]
                    ]
                ]
            

            -- Hauptinhalt der Applikation wird über contentView geladen--
            -- grow-1: soll den ganzen Raum zwischen Header und Footer einnehmen
            , section [ class "section is-flex-grow-1" ]
                [ div [ class "container" ] [ contentView model ] ]
            

            -- Footer mit Bulma-Footer, Padding 3 auf y-Achse --
            , footer [ class "footer py-3"]
                [ div [ class "content" ]
                    [ p [] [ text "Projekt für das WWW-Modul SoSe26, von Luke-Felix Brüske und Katherina Shapilova" ]
                    , p [] [ a [ href "https://github.com/lfb241/TaxoView", class "button is-black" ] [ text "Github-Code" ]]
                    ]
                ]
            ]
        ]
    }


contentView : Model -> Html Msg
contentView model =
    case model.state of
        -- Homepage/Landingpage
        Home home ->
            div []
                -- Box von bulma
                [ div [ class "box" ]
                    -- marginbottom 2
                    [ p [ class "mb-2" ]
                        [ text "Gib einen Suchbegriff ein, um biologische Organismen in der Datenbank zu finden (derzeit Primaten und Katzen als Beispieldaten)." ]
                    , p []
                        [ text "Klicke anschließend auf 'Visualisierung', um den zugehörigen Baum zu öffnen." ]
                    ]

                -- Suchfeld
                , form [ onSubmit SubmitForm ]
                    -- Inhalt ist im Center der x-Achse, field gruppiert controls
                    -- has-addons ermöglicht Button rechts
                    [ div [ class "field has-addons is-justify-content-center" ]
                        -- is-expanded verbreitert Form
                        [ div [ class "control is-expanded" ]
                            [ input
                                [ class "input is-link"
                                , type_ "text"
                                , placeholder "Suche in Datenbank nach biologischen Entitäten..."
                                , value home.formString
                                , onInput UpdateFormString
                                ]
                                []
 
                            ]

                        , div [ class "control" ]
                            [ button
                                [ class "button is-link", type_ "submit" ]
                                [ text "Suchen" ]
                            ]
                        ]
                    ]

                -- Ergebnis-Tabelle
                , div [ class "container mt-4" ]
                    -- Konditionelles Rendering
                    (if home.showResultTable then
                        -- wieder Bulma-Box
                        [ div [ class "table-container box" ]
                    
                            [ table [ class "table is-striped is-hoverable is-fullwidth" ]
                                [ thead []
                                    [ tr []

                                        [ th [] [ text "Biologische Entitäten:" ]
                                        -- zweite Header-Cell für Button
                                        , th [] []
                                        ]
                                    ]
                                , tbody []
                                    -- Konditionelles Rendering, je nachdem ob Ergebnisse gefunden wurden
                                    
                                    (if List.isEmpty (Maybe.withDefault [] home.results) then
                                        [ tr []
                                            [ td [ class "has-text-centered" ]
                                                -- Bulma-Klasse notification
                                                [ div [ class "notification is-warning is-light" ]
                                                    [ text "Keine Einträge gefunden... Versuche es nochmal!" ]
                                                ]
                                            , td [] []
                                            ]
                                        ]
                                    -- wenn Ergebnisse gefunden wurden
                                     else
                                        List.map
                                            (\x ->
                                                tr []
                                                    -- vertikal zentriert
                                                    [ td [ class "is-vcentered" ] [ text x ]
                                                    -- button nach rechts
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
            div []
                [ div [ class "box" ]
                    -- is-5/6 sind Klassen für Textgrößen
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


-- Zur Anzeige der Metadaten in Tabelle
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
                            (metadataToPairs metadataList)
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
                    -- weitersuchen in Geschwisterknoten
                    Nothing ->
                        findNode targetId rest


-- Methode um Substrings eines String in Keys von Key-Value-Paaren zu finden
findMatchingValues : String -> List ( String, String ) -> List String
findMatchingValues query pairs =
    case query of
        "" ->
            []
        _ -> 
            pairs
                |> List.filter (\( key, _ ) -> String.contains query key)
                |> List.map (\( _, value ) -> value)
