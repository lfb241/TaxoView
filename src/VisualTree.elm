--elm install gampleman/elm-visualization
module VisualTree exposing (draw)

import Html exposing (Html)
import Tree exposing (TreeNode(..))
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg.Core exposing (attribute)
import TypedSvg.Events exposing (onClick)


type alias LayoutNode =
    { node : TreeNode
    , label : String
    , rank : String
    , x : Float
    , y : Float
    , children : List LayoutNode
    }


leafSpacing : Float
leafSpacing =
    120


levelSpacing : Float
levelSpacing =
    100


paddingX : Float
paddingX =
    80


paddingY : Float
paddingY =
    70


-- Erstellt einen responsiven SVG-Baum
-- Die Koordinaten der Knoten werden nach einem „Tidy“-Layout berechnet:
-- Blattknoten werden gleichmäßig verteilt, und übergeordnete Knoten werden über ihren untergeordneten Knoten zentriert
draw : (TreeNode -> msg) -> Maybe (List TreeNode) -> Html msg
draw onSelect maybeTree =
    case maybeTree of
        Nothing ->
            Html.div [] [ Html.text "No Data" ]

        Just [] ->
            Html.div [] [ Html.text "Empty Tree" ]

        Just roots ->
            let
                ( _, layoutRoots ) =
                    layoutForest 0 0 roots

                leafCount =
                    List.sum (List.map countLeaves roots)

                maxDepth =
                    List.maximum (List.map treeDepth roots)
                        |> Maybe.withDefault 1

                svgWidth =
                    max 1200 (paddingX * 2 + toFloat leafCount * leafSpacing)

                svgHeight =
                    max 800 (paddingY * 2 + toFloat maxDepth * levelSpacing)
            in
            svg
                [ viewBox 0 0 svgWidth svgHeight
                , attribute "class" "tree-svg"
                , attribute "preserveAspectRatio" "xMidYMin meet"
                ]
                (List.concatMap renderLinks layoutRoots
                    ++ List.concatMap (flatten >> List.map (renderNode onSelect)) layoutRoots
                )


-- Berechnet das Layout für eine Liste von root nodes
layoutForest : Int -> Float -> List TreeNode -> ( Float, List LayoutNode )
layoutForest depth nextLeaf nodes =
    case nodes of
        [] ->
            ( nextLeaf, [] )

        node :: rest ->
            let
                ( nextAfterNode, layoutNode ) =
                    layoutTree depth nextLeaf node

                ( finalNext, layoutRest ) =
                    layoutForest depth nextAfterNode rest
            in
            ( finalNext, layoutNode :: layoutRest )


-- Berechnet das Layout für einen Knoten
-- Blattknoten erhalten die nächste freie X-Position
-- Innenknoten werden über ihren untergeordneten Knoten zentriert
layoutTree : Int -> Float -> TreeNode -> ( Float, LayoutNode )
layoutTree depth nextLeaf node =
    let
        ( label, rank, rawChildren ) =
            case node of
                TreeNode _ nodeLabel nodeRank _ maybeChildren ->
                    ( nodeLabel, nodeRank, Maybe.withDefault [] maybeChildren )

        yPos =
            paddingY + toFloat depth * levelSpacing
    in
    case rawChildren of
        [] ->
            let
                xPos =
                    paddingX + nextLeaf * leafSpacing
            in
            ( nextLeaf + 1
            , { node = node
              , label = label
              , rank = rank
              , x = xPos
              , y = yPos
              , children = []
              }
            )

        _ ->
            let
                ( nextAfterChildren, layoutChildren ) =
                    layoutForest (depth + 1) nextLeaf rawChildren

                childXs =
                    List.map .x layoutChildren

                xPos =
                    case childXs of
                        [] ->
                            paddingX + nextLeaf * leafSpacing

                        _ ->
                            List.sum childXs / toFloat (List.length childXs)
            in
            ( nextAfterChildren
            , { node = node
              , label = label
              , rank = rank
              , x = xPos
              , y = yPos
              , children = layoutChildren
              }
            )


renderLinks : LayoutNode -> List (Html msg)
renderLinks parent =
    let
        directLinks =
            List.map
                (\child ->
                    line
                        [ attribute "x1" (String.fromFloat parent.x)
                        , attribute "y1" (String.fromFloat parent.y)
                        , attribute "x2" (String.fromFloat child.x)
                        , attribute "y2" (String.fromFloat child.y)
                        , attribute "stroke" "#dee2e6"
                        , attribute "stroke-width" "2"
                        ]
                        []
                )
                parent.children
    in
    directLinks ++ List.concatMap renderLinks parent.children


renderNode : (TreeNode -> msg) -> LayoutNode -> Html msg
renderNode onSelect item =
    g []
        [ circle
            [ attribute "cx" (String.fromFloat item.x)
            , attribute "cy" (String.fromFloat item.y)
            , attribute "r" "16"
            , attribute "fill" (colorForRank item.rank)
            , attribute "cursor" "pointer"
            , onClick (onSelect item.node)
            ]
            []
        , text_
            [ attribute "x" (String.fromFloat item.x)
            , attribute "y" (String.fromFloat (item.y - 24))
            , attribute "font-size" "13"
            , attribute "text-anchor" "middle"
            , attribute "fill" "#343a40"
            ]
            [ Html.text item.label ]
        ]


flatten : LayoutNode -> List LayoutNode
flatten node =
    node :: List.concatMap flatten node.children


countLeaves : TreeNode -> Int
countLeaves node =
    case node of
        TreeNode _ _ _ _ maybeChildren ->
            case Maybe.withDefault [] maybeChildren of
                [] ->
                    1

                children ->
                    List.sum (List.map countLeaves children)


treeDepth : TreeNode -> Int
treeDepth node =
    case node of
        TreeNode _ _ _ _ maybeChildren ->
            case Maybe.withDefault [] maybeChildren of
                [] ->
                    1

                children ->
                    1 + (List.maximum (List.map treeDepth children) |> Maybe.withDefault 0)


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
