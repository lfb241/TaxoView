module Tree exposing (Metadata(..),TreeNode(..), decodeTreeString, metadataToList)
import Json.Decode exposing (Decoder, field, string, list, maybe, lazy, map3, map5)


-- Typ für Metadata, kann bei Bedarf erweitert werden
type Metadata =
  ScientificName String
  | Description String
  | CommonName String



--- Typ für Baumknoten: id, label, rank, metadata, children
type TreeNode =
  TreeNode String String String (Maybe (List Metadata)) (Maybe (List TreeNode))


{-
TODO:
- mehr Metadaten einbauen?
- Methode metadataToList implementieren
-}

-- soll Metadaten als key-value-pair anzeigen
metadataToList: List(Metadata) -> List(String, String)
metadataToList metadata = [("Test","Test")]

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
        
-- Methode zur Nutzung des Decoders
decodeTreeString : String -> Result Json.Decode.Error (List TreeNode)
decodeTreeString jsonString =
    Json.Decode.decodeString treeDecoder jsonString
        -- root to list
        |> Result.map (\node -> [ node ])