module TaxonTree exposing (Metadata(..),TreeNode(..), decodeTreeString, metadataToPairs)
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

-- Liste von Metadaten wird als key-value-pair ausgegeben
metadataToPairs : List Metadata -> List ( String, String )
metadataToPairs metadata =
    List.map
        (\m ->
            -- Pattern matching hochspezialisiert auf die Metadaten unserer Beispiele
            case m of
                ScientificName value ->
                    ( "Wissenschaftlicher Name", value )

                CommonName value ->
                    ( "Trivialname", value )

                Description value ->
                    ( "Beschreibung", value )
        )
        metadata
        
--Decoder for metadata
metadataDecoder : Decoder (Maybe (List Metadata))
metadataDecoder =
    Decode.maybe
        (Decode.field "metadata"
            (Decode.map3
                (\sci common desc ->
                    List.filterMap identity
                        [ Maybe.map ScientificName sci
                        , Maybe.map CommonName common
                        , Maybe.map Description desc
                        ]
                )
                (Decode.maybe (Decode.field "scientificName" Decode.string))
                (Decode.maybe (Decode.field "commonName" Decode.string))
                (Decode.maybe (Decode.field "description" Decode.string))
            )
        )
        
--Decoder for TreeNode
treeDecoder : Decoder TreeNode
treeDecoder =
    Decode.map5 TreeNode
        (Decode.field "id" Decode.string)
        (Decode.field "label" Decode.string)
        (Decode.field "rank" Decode.string)
        metadataDecoder
        -- lazy vermeidet unendliche Rekursion
        (Decode.maybe (Decode.field "children" (Decode.list (Decode.lazy (\_ -> treeDecoder)))))
        
-- Methode zur Nutzung des Decoders
decodeTreeString : String -> Result Decode.Error (List TreeNode)
decodeTreeString jsonString =
    Decode.decodeString treeDecoder jsonString
        -- root to list
        |> Result.map (\node -> [ node ])
