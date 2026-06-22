module VisualTree exposing (draw)

import Html exposing (Html)
import Tree exposing (TreeNode(..))
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg.Attributes.InNamespace exposing (attr)
import TypedSvg.Events exposing (onClick)


draw : (TreeNode -> msg) -> Maybe (List TreeNode) -> Html msg
draw onSelect maybeTree =
    case maybeTree of
        Nothing ->
            Html.div [] [ Html.text "No Data" ]

        Just [] ->
            Html.div [] [ Html.text "Empty Tree" ]

        Just roots ->
            svg [ viewBox 0 0 800 400 ]
                (List.indexedMap
                    (\index rootNode ->
                        renderTree onSelect rootNode (200 + toFloat index * 250) 50 1
                    )
                    roots
                )


renderTree : (TreeNode -> msg) -> TreeNode -> Float -> Float -> Int -> Html msg
renderTree onSelect node xPos yPos level =
    let
        TreeNode id label rank maybeMeta maybeChildren =
            node

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
                                    [ attr "x1" (String.fromFloat xPos)
                                    , attr "y1" (String.fromFloat yPos)
                                    , attr "x2" (String.fromFloat childX)
                                    , attr "y2" (String.fromFloat childY)
                                    , attr "stroke" "#dee2e6"
                                    ]
                                    []
                                , renderTree onSelect child childX childY (level + 1)
                                ]
                        )
                        children
    in
    g []
        ([ circle
            [ attr "cx" (String.fromFloat xPos)
            , attr "cy" (String.fromFloat yPos)
            , attr "r" "15"
            , attr "fill" nodeColor
            , attr "cursor" "pointer"
            , onClick (onSelect node)
            ]
            []
         , text_
            [ attr "x" (String.fromFloat xPos)
            , attr "y" (String.fromFloat (yPos - 20))
            , attr "font-size" "12"
            , attr "text-anchor" "middle"
            , attr "fill" "#343a40"
            ]
            [ Html.text label ]
         ]
            ++ childrenSvg
        )
