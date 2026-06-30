module Metadata exposing (..)
type Metadata =
  ScientificName String
  | Description String
  | CommonName String

{-
TODO:
- mehr Metadaten einbauen?
- Methode toPair implementieren
-}

-- soll Metadaten als key-value-pair anzeigen
toPair: Metadata -> (String, String)
toPair metadata = ("Test","Test")