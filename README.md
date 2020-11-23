Click on data.tsv to see the urban areas of the world with >= 500,000 people.

It was generated (by the included Haskell script) from the Demographia data on
the urban areas of the world with >= 500,000 people:
- run: pdftotext db-worldua.pdf
- That makes db-worldua.txt.
- run: runhaskell p.hs
- That makes data.tsv.

Note that these are based on urban areas, not administrative city boundaries
(which are usually smaller) nor metro areas (which are usually bigger).
https://en.wikipedia.org/wiki/List_of_largest_cities#Urban_area
