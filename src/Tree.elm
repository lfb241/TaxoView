module Tree exposing (TreeNode(..), decodeTreeString, toString)
import Metadata exposing(Metadata(..))
import String exposing (repeat)

import Json.Decode exposing (Decoder, field, string, list, maybe, lazy, map3, map5)

--- id, label, rank, metadata, children
type TreeNode =
  TreeNode String String String (Maybe (List Metadata)) (Maybe (List TreeNode))


-- TODO: Handle decoding
-- decodeTreeString : String -> Result Json.Decode.Error (List(TreeNode))

--Decoder for metadata
metadataDecoder : Decoder (Maybe (List Metadata))
metadataDecoder =
    maybe
        (field "metadata"
            (map3
                (\sci common desc ->
                    List.filterMap identity
                        [ Maybe.map ScientificName sci
                        , Maybe.map CommonName common
                        , Maybe.map Description desc
                        ]
                )
                (maybe (field "scientificName" string))
                (maybe (field "commonName" string))
                (maybe (field "description" string))
            )
        )
        
--Decoder for TreeNode
treeDecoder : Decoder TreeNode
treeDecoder =
    map5 TreeNode
        (field "id" string)
        (field "label" string)
        (field "rank" string)
        metadataDecoder
        -- lazy vermeidet unendliche Rekursion
        (maybe (field "children" (list (lazy (\_ -> treeDecoder)))))
        
--aaaaaaaaaaaaaaa
decodeTreeString : String -> Result Json.Decode.Error (List TreeNode)
decodeTreeString jsonString =
    Json.Decode.decodeString treeDecoder jsonString
        -- root to list
        |> Result.map (\node -> [ node ])


--- Fürs Testen

toString : List TreeNode -> String
toString nodes =
    String.join "" (List.map (treeNodeToString 0) nodes)

treeNodeToString : Int -> TreeNode -> String
treeNodeToString indent (TreeNode id label rank metadata children) =
    let
        ind = repeat indent "  "
        metaStr =
            case metadata of
                Nothing ->
                    ""

                Just ms ->
                    " [" ++ (String.join ", " (List.map metadataToString ms)) ++ "]"

        header =
            ind ++ label ++ " (" ++ rank ++ ") - " ++ id ++ metaStr ++ "\n"

        childrenStr =
            case children of
                Nothing ->
                    ""

                Just cs ->
                    String.join "" (List.map (treeNodeToString (indent + 1)) cs)
    in
    header ++ childrenStr


metadataToString : Metadata -> String
metadataToString meta =
    case meta of
        ScientificName s ->
            "scientific: " ++ s

        CommonName s ->
            "common: " ++ s

        Description s ->
            "desc: " ++ s
