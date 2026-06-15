## WWW-Modulprojekt SoSe26

### Projektidee

SVG-Top-Down-Darstellung von biologischen Taxonomien (Daten von: https://www.gbif.org/species/search, eventuell müssen Anfragen beschränkt werden wenn Daten sehr groß). Der User kann per Textsuche eine Taxonomie aussuchen. Die URL soll den aktuell geladenen Taxon-Key und den ausgewählten Knoten beinhalten. Beim Klick auf einen Knoten werden Metadaten angezeigt und optional zusätzliche Informationen per HTTP nachgeladen (eventuell kann man auch Kinderknoten "ausklappen/erweitern").

### To-Do

- Read https://guide.elm-lang.org/webapps/structure
- Define HTML/CSS-Seitenlayouts (Was kann die Seite? Wie sieht sie aus?)
- Define JSON.Decoder for Trees (Wie kommen wir von JSON zu Elm-Daten?)
- Define SVG-Tree-Darstellung using elm-visualization (Wie kommen wir von Elm-Daten zu Baumdarstellung?)
- Add Metadata to example data (Um Metadaten-Modal bei Knotenklick aufzurufen)
