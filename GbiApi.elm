module GbifApi exposing (loadChildren)

import Http
import Json.Decode as Decode exposing (Decoder)
import Tree exposing (TreeNode(..))


loadChildren : String -> (Result Http.Error (List TreeNode) -> msg) -> Cmd msg
loadChildren taxonKey toMsg =
    Http.get
        { url = "https://api.gbif.org/v1/species/" ++ taxonKey ++ "/children?limit=20"
        , expect = Http.expectJson toMsg childrenDecoder
        }


childrenDecoder : Decoder (List TreeNode)
childrenDecoder =
    Decode.field "results" (Decode.list gbifNodeDecoder)


gbifNodeDecoder : Decoder TreeNode
gbifNodeDecoder =
    Decode.map5 TreeNode
        (Decode.field "key" Decode.int |> Decode.map String.fromInt)
        (Decode.oneOf
            [ Decode.field "canonicalName" Decode.string
            , Decode.field "scientificName" Decode.string
            ]
        )
        (Decode.field "rank" Decode.string)
        (Decode.succeed Nothing)
        (Decode.succeed Nothing)
