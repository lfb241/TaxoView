module Route exposing (Route(..), parseUrl)
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>),(<?>), top, map, oneOf, s, string, parse)
import Url.Parser.Query as Query
import String

type Route
  = 
  Home
  | Search (Maybe String)
  | Tree String
  | Node String String


-- top ist Homepage
routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Node (s "tree" </> string </> s "node" </> string)
        , map Tree (s "tree" </> string)
        , map Search (s "search" <?> Query.string "query")
        , map Home top
        ]

-- Methode zur Parserbenutzung
parseUrl : Url -> Route
parseUrl url =
    let
        fragment =
            Maybe.withDefault "" url.fragment

        ( path, query ) =
            case String.split "?" fragment of
                [ onlyPath ] ->
                    ( onlyPath, Nothing )

                onlyPath :: rest ->
                    ( onlyPath, Just (String.join "?" rest) )

                [] ->
                    ( "", Nothing )
    in
    { url
        | path = path
        , query = query
        , fragment = Nothing
    }
        |> parse routeParser
        |> Maybe.withDefault Home
-- Beispiele

-- /TaxoView --> Just (Home)
-- /TaxoView/search?query=test --> Just(Search "test")
-- /TaxoView/tree/primates --> Just(Tree "primates")
-- /tree/primates/node/Human --> Just(Node "primates" "human")