module Metadata exposing (..)

type Metadata =
  ScientificName String
  | Description String
  | CommonName String



--- TODO: implement getMetadata

-- getMetaData: String -> List(MetaData)
-- getMetaData _ =

toPair: Metadata -> (String, String)
toPair metadata = ("Test","Test")