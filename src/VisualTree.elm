module VisualTree exposing (draw)

import Html exposing (Html)
import Tree exposing (TreeNode(..))
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg.Core exposing (attribute)
import TypedSvg.Events exposing (onClick)

-- Methode macht aus TreeNode-Liste ein HTML-Objekt
-- TODO: responsive machen
draw : (TreeNode -> msg) -> Maybe (List TreeNode) -> Html msg
draw onSelect maybeTree =
    case maybeTree of
        Nothing ->
            Html.div [] [ Html.text "No Data" ]

        Just [] ->
            Html.div [] [ Html.text "Empty Tree" ]

        Just roots ->
            svg [ viewBox 0 0 1200 800 ]
                (List.indexedMap
                    (\index rootNode ->
                        renderTree onSelect rootNode (200 + toFloat index * 250) 50 1
                    )
                    roots
                )

-- Hilfsmethode zur SVG-Generierung TODO: add comments
renderTree : (TreeNode -> msg) -> TreeNode -> Float -> Float -> Int -> Html msg
renderTree onSelect node xPos yPos level =
    let
        ( label, rank, maybeChildren ) =
            case node of
                TreeNode _ nodeLabel nodeRank _ nodeChildren ->
                    ( nodeLabel, nodeRank, nodeChildren )

        -- TODO: hier fehlen noch Farben für Rank = order, suborder, infraorder
        nodeColor =
            case String.toLower rank of
                "family" ->
                    "#ff6b6b"

                "subfamily" ->
                    "#4dadf7"

                "genus" ->
                    "#51cf66"

                "species" ->
                    "#fcc419"

                _ ->
                    "#adb5bd"

        childrenSvg =
            case maybeChildren of
                Nothing ->
                    []

                Just children ->
                    List.indexedMap
                        (\index child ->
                            let
                                childX =
                                    xPos + (toFloat index - (toFloat (List.length children - 1) / 2)) * (180 / toFloat level)

                                childY =
                                    yPos + 80
                            in
                            g []
                                [ line
                                    [ attribute "x1" (String.fromFloat xPos)
                                    , attribute "y1" (String.fromFloat yPos)
                                    , attribute "x2" (String.fromFloat childX)
                                    , attribute "y2" (String.fromFloat childY)
                                    , attribute "stroke" "#dee2e6"
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
            , attribute "r" "15"
            , attribute "fill" nodeColor
            , attribute "cursor" "pointer"
            , onClick (onSelect node)
            ]
            []
         , text_
            [ attribute "x" (String.fromFloat xPos)
            , attribute "y" (String.fromFloat (yPos - 20))
            , attribute "font-size" "12"
            , attribute "text-anchor" "middle"
            , attribute "fill" "#343a40"
            ]
            [ Html.text label ]
         ]
            ++ childrenSvg
        )