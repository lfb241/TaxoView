--elm install gampleman/elm-visualization
module VisualTree exposing (draw)

import Hierarchy
import Html exposing (Html)
import TaxonTree exposing (TreeNode(..))
import Tree as RoseTree exposing (Tree)
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg.Core exposing (attribute)
import TypedSvg.Events exposing (onClick)

-- Speichert alle Informationen, die für die Darstellung eines Knotens
--         im SVG benötigt werden (Position, Größe und zugehörige Taxonomie)
type alias NodeData =
    { taxon : TreeNode
    , label : String
    , rank : String
    , width : Float
    , height : Float
    }

-- Abstand zwischen dem Baum und dem Rand des SVG
padding : Float
padding =
    80


-- Hauptfunktion zur Darstellung eines phylogenetischen Baumes
-- Die Baumstruktur wird zunächst in einen RoseTree umgewandelt und anschließend
--          mit dem Tidy-Layout-Algorithmus (Hierarchy.tidy) positioniert
-- Dadurch werden überlappende Knoten vermieden
draw : (TreeNode -> msg) -> Maybe (List TreeNode) -> Html msg
draw onSelect maybeTree =
    case maybeTree of
        Nothing ->
            Html.div [] [ Html.text "No Data" ]

        Just [] ->
            Html.div [] [ Html.text "Baum ist leer" ]

        Just roots ->
            let
                tidyRoots =
                    List.map (toRoseTree >> tidyLayout) roots

                allNodes =
                    List.concatMap RoseTree.toList tidyRoots

                minX =
                    List.minimum (List.map .x allNodes) |> Maybe.withDefault 0

                maxX =
                    List.maximum (List.map (\n -> n.x + n.width) allNodes) |> Maybe.withDefault 1200

                maxY =
                    List.maximum (List.map (\n -> n.y + n.height) allNodes) |> Maybe.withDefault 800

                offsetX =
                    padding - minX

                offsetY =
                    padding

                svgWidth =
                    max 1200 (maxX - minX + padding * 2)

                svgHeight =
                    max 800 (maxY + padding * 2)
            in
            svg
                [ viewBox 0 0 svgWidth svgHeight
                , attribute "class" "tree-svg"
                , attribute "preserveAspectRatio" "xMidYMin meet"
                ]
                (List.concatMap (renderLinks offsetX offsetY) tidyRoots
                    ++ List.concatMap
                        (\root ->
                            root
                                |> RoseTree.toList
                                |> List.map (renderNode onSelect offsetX offsetY)
                        )
                        tidyRoots
                )


-- Konvertiert unsere eigene TreeNode-Struktur in einen RoseTree,
--          der vom Hierarchy-Modul verarbeitet werden kann
toRoseTree : TreeNode -> Tree NodeData
toRoseTree taxon =
    case taxon of
        TreeNode _ label rank _ maybeChildren ->
            let
                children =
                    Maybe.withDefault [] maybeChildren

                data =
                    { taxon = taxon
                    , label = label
                    , rank = rank
                    , width = nodeWidth label
                    , height = 50
                    }
            in
            case children of
                [] ->
                    RoseTree.singleton data

                _ ->
                    RoseTree.tree data (List.map toRoseTree children)

-- Berechnet automatisch die Position jedes Knotens mithilfe
--         des Tidy-Layout-Algorithmus aus elm-visualization
tidyLayout :
    Tree NodeData
    -> Tree { x : Float, y : Float, width : Float, height : Float, node : NodeData }
tidyLayout tree =
    Hierarchy.tidy
        [ Hierarchy.nodeSize
            (\node ->
                ( node.width, node.height )
            )
        , Hierarchy.parentChildMargin 70
        , Hierarchy.peerMargin 35
        , Hierarchy.layered
        ]
        tree

-- Zeichnet alle Verbindungslinien zwischen Eltern- und Kindknoten
renderLinks :
    Float
    -> Float
    -> Tree { x : Float, y : Float, width : Float, height : Float, node : NodeData }
    -> List (Html msg)
renderLinks offsetX offsetY tree =
    tree
        |> RoseTree.links
        |> List.map
            (\( from, to ) ->
                line
                    [ attribute "x1" (String.fromFloat (offsetX + from.x + from.width / 2))
                    , attribute "y1" (String.fromFloat (offsetY + from.y + from.height))
                    , attribute "x2" (String.fromFloat (offsetX + to.x + to.width / 2))
                    , attribute "y2" (String.fromFloat (offsetY + to.y))
                    , attribute "stroke" "#dee2e6"
                    , attribute "stroke-width" "2"
                    ]
                    []
            )

-- Zeichnet einen einzelnen Knoten inklusive Beschriftung
-- Ein Klick auf den Knoten löst die Anzeige der Metadaten aus
renderNode :
    (TreeNode -> msg)
    -> Float
    -> Float
    -> { x : Float, y : Float, width : Float, height : Float, node : NodeData }
    -> Html msg
renderNode onSelect offsetX offsetY item =
    let
        centerX =
            offsetX + item.x + item.width / 2

        centerY =
            offsetY + item.y + item.height / 2
    in
    g []
        [ circle
            [ attribute "cx" (String.fromFloat centerX)
            , attribute "cy" (String.fromFloat centerY)
            , attribute "r" "16"
            , attribute "fill" (colorForRank item.node.rank)
            , attribute "cursor" "pointer"
            , onClick (onSelect item.node.taxon)
            ]
            []
        , text_
            [ attribute "x" (String.fromFloat centerX)
            , attribute "y" (String.fromFloat (centerY - 24))
            , attribute "font-size" "13"
            , attribute "text-anchor" "middle"
            , attribute "fill" "#343a40"
            ]
            [ Html.text item.node.label ]
        ]

-- Berechnet die minimale Breite eines Knotens abhängig von der Länge der Beschriftung
nodeWidth : String -> Float
nodeWidth label =
    max 90 (toFloat (String.length label) * 8 + 30)

-- Ordnet den verschiedenen taxonomischen Rängen unterschiedliche Farben zu
colorForRank : String -> String
colorForRank rank =
    case String.toLower rank of
        "order" ->
            "#845ef7"

        "suborder" ->
            "#339af0"

        "infraorder" ->
            "#22b8cf"

        "family" ->
            "#ff6b6b"

        "subfamily" ->
            "#4dabf7"

        "genus" ->
            "#51cf66"

        "species" ->
            "#fcc419"

        _ ->
            "#adb5bd"
