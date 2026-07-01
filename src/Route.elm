module Route exposing (Route(..), parseUrl)
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>),(<?>), top, map, oneOf, s, string, parse)
import Url.Parser.Query as Query

type Route
  = 
  Home
  | Search (Maybe String)
  | Tree String
  | Node String String


-- top ist Homepage
routeParser : Parser (Route -> a) a
routeParser =
  s "TaxoView" </>
  oneOf
    [
     map Node (s "tree" </> string </> s "node" </> string)
    , map Tree (s "tree" </> string)
    , map Search (s "search" <?> Query.string "query")
    ,  map Home top
    ]

-- Methode zur Parserbenutzung
parseUrl : Url -> Route
parseUrl url =
    parse routeParser url
        |> Maybe.withDefault Home

-- Beispiele

-- /TaxoView --> Just (Home)
-- /TaxoView/search?query=test --> Just(Search "test")
-- /TaxoView/tree/primates --> Just(Tree "primates")
-- /tree/primates/node/Human --> Just(Node "primates" "human")