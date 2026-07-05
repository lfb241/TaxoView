module VisualTree exposing (draw)

import Html exposing (Html)
import Tree exposing (TreeNode(..))
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg.Core exposing (attribute)
import TypedSvg.Events exposing (onClick)

-- Methode macht aus TreeNode-Liste ein HTML-Objekt
-- SVG nutzt viewBox -> zu verfügbare Seitenbreite angepasst
draw : (TreeNode -> msg) -> Maybe (List TreeNode) -> Html msg
draw onSelect maybeTree =
    case maybeTree of
        Nothing ->
            Html.div [] [ Html.text "No Data" ]

        Just [] ->
            Html.div [] [ Html.text "Empty Tree" ]

        Just roots ->
            svg
                [ viewBox 0 0 1200 800
                , attribute "class" "tree-svg"
                , attribute "preserveAspectRatio" "xMidYMin meet"
                ]
                (List.indexedMap
                    (\index rootNode ->
                        renderTree onSelect rootNode (600 + toFloat index * 300) 60 1
                    )
                    roots
                )

-- Hilfsmethode zur SVG-Generierung
-- Ein Knoten und alle seine untergeordneten Knoten werden rekursiv dargestellt.
-- Jeder Knoten wird als farbiger Kreis mit einer Beschriftung darüber dargestellt.
-- Eltern-Kind-Beziehungen werden als SVG-Linien dargestellt.
renderTree : (TreeNode -> msg) -> TreeNode -> Float -> Float -> Int -> Html msg
renderTree onSelect node xPos yPos level =
    let
        ( label, rank, maybeChildren ) =
            case node of
                TreeNode _ nodeLabel nodeRank _ nodeChildren ->
                    ( nodeLabel, nodeRank, nodeChildren )

        nodeColor =
            colorForRank rank

        childrenSvg =
            case maybeChildren of
                Nothing ->
                    []

                Just children ->
                    let
                        childCount =
                            List.length children

                        spacing =
                            max 90 (280 / toFloat level)
                    in
                    List.indexedMap
                        (\index child ->
                            let
                                childX =
                                    xPos
                                        + (toFloat index - (toFloat (childCount - 1) / 2))
                                        * spacing

                                childY =
                                    yPos + 95
                            in
                            g []
                                [ line
                                    [ attribute "x1" (String.fromFloat xPos)
                                    , attribute "y1" (String.fromFloat yPos)
                                    , attribute "x2" (String.fromFloat childX)
                                    , attribute "y2" (String.fromFloat childY)
                                    , attribute "stroke" "#dee2e6"
                                    , attribute "stroke-width" "2"
                                    ]
                                    []
                                , renderTree onSelect child childX childY (level + 1)
                                ]
                        )
                        children
    in
    g []
        ([ circle
            [ attribute "cx" (String.fromFloat xPos)
            , attribute "cy" (String.fromFloat yPos)
            , attribute "r" "16"
            , attribute "fill" nodeColor
            , attribute "cursor" "pointer"
            , onClick (onSelect node)
            ]
            []
         , text_
            [ attribute "x" (String.fromFloat xPos)
            , attribute "y" (String.fromFloat (yPos - 20))
            , attribute "font-size" "13"
            , attribute "text-anchor" "middle"
            , attribute "fill" "#343a40"
            ]
            [ Html.text label ]
         ]
            ++ childrenSvg
        )
        
-- Weist den taxonomischen Rängen Farben zu
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
