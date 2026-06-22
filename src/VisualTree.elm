module VisualTree exposing (draw)

import Html exposing (Html)
import Tree exposing (TreeNode(..))
import TypedSvg exposing (svg, circle, line, text_, g)
import TypedSvg.Attributes exposing (cx, cy, r, fill, stroke, x, y, fontSize, viewBox)
import TypedSvg.Attributes.InNamespace exposing (attr)
import TypedSvg.Events exposing (onClick)


draw : (TreeNode -> msg) -> Maybe (List TreeNode) -> Html msg
draw onSelect maybeTree =
    case maybeTree of
        Nothing ->
            Html.div [] [ Html.text "No Data" ]

        Just [] ->
            Html.div [] [ Html.text "Empty Tree" ]

        Just (rootNode :: _) ->
            svg [ viewBox 0 0 800 400 ]
                [ renderTree onSelect rootNode 400 50 1 ]


renderTree : (TreeNode -> msg) -> TreeNode -> Float -> Float -> Int -> Html msg
renderTree onSelect (TreeNode id label rank maybeMeta maybeChildren) xPos yPos level =
    let
        nodeColor =
            case rank of
                "family" ->
                    "#ff6b6b"

                "subfamily" ->
                    "#4dadf7"

                "genus" ->
                    "#51cf66"

                _ ->
                    "#fcc419"

        childrenSvg =
            case maybeChildren of
                Nothing ->
                    []

                Just children ->
                    List.indexedMap
                        (\index child ->
                            let
                                childX =
                                    xPos + (toFloat index - (toFloat (List.length children - 1) / 2)) * (150 / toFloat level)

                                childY =
                                    yPos + 80
                            in
                            g []
                                [ line
                                    [ attr "x1" (String.fromFloat xPos)
                                    , attr "y1" (String.fromFloat yPos)
                                    , attr "x2" (String.fromFloat childX)
                                    , attr "y2" (String.fromFloat childY)
                                    , stroke "#dee2e6"
                                    ]
                                    []
                                , renderTree onSelect child childX childY (level + 1)
                                ]
                        )
                        children
    in
    g []
        ([ circle
            [ cx xPos
            , cy yPos
            , r 15
            , fill nodeColor
            , onClick (onSelect (TreeNode id label rank maybeMeta maybeChildren))
            ]
            []
         , text_
            [ x xPos
            , y (yPos - 20)
            , fontSize "12"
            , attr "text-anchor" "middle"
            , fill "#343a40"
            ]
            [ Html.text label ]
         ]
            ++ childrenSvg
        )
