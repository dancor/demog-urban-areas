This is a program to manipulate Demographia data on the urban areas of the
world.  The data is supposed to have all of them
in the world with >= 500k people, and for certain countries has additional data
(e.g. all the way down to >= 5k in AU).

Steps used to process the 2020 data:
- Download 2020 db-worldua.pdf
- run: pdftotext db-worldua.pdf
- run: <db-worldua.txt runhaskell src/ExtractPdf.hs

For "urban area" vs "metro area":
http://en.wikipedia.org/wiki/List_of_urban_areas_by_population#Definitions_and_issues

