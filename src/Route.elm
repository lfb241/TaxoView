module Route exposing (Route(..), parseUrl)
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), top, map, oneOf, s, string, parse)



type Route
  = 
  Top
  | Tree String
  | Node String String

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map Top top
    , map Node (s "tree" </> string </> s "node" </> string)
    , map Tree (s "tree" </> string)
    ]


parseUrl : Url -> Route
parseUrl url =
    parse routeParser url
        |> Maybe.withDefault Top

-- /tree/primates --> Just(Tree "primates")
-- /tree/primates/human --> Just(Node "primates" "human")