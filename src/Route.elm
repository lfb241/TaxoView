module Route exposing (Route(..), parseUrl)
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), top, map, oneOf, s, string, parse)

type Route
  = 
  Top
  | Tree String
  | Node String String

-- top ist Homepage
routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [
     map Node (s "tree" </> string </> s "node" </> string)
    , map Tree (s "tree" </> string)
    ,  map Top top
    ]

-- Methode zur Parserbenutzung
parseUrl : Url -> Route
parseUrl url =
    parse routeParser url
        |> Maybe.withDefault Top

-- Beispiele
-- /tree/primates --> Just(Tree "primates")
-- /tree/primates/node/Human --> Just(Node "primates" "human")