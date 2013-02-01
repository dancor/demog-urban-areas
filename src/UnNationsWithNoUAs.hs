#include <h>

main = do
    un1L <- lines <$> readFile "/home/danl/p/demog_urban_areas/output/un1"
    unAllL <- lines <$> readFile "/home/danl/n/world/un_nations" 
    let un1L' = map (takeWhile (/= ':') . drop 1 . dropWhile (/= ' ')) un1L
        unAllL' =
            map (replace "Central African Republic" "CAR") $
            map (replace "Republic of the Congo" "RCongo") $
            map (replace "Democratic Republic of the Congo" "DRCongo") $
            map (replace "United Arab Emirates" "UAE") $
            map (replace "United Kingdom" "UK") $
            map (replace "United States of America" "USA") $
            map (replace "Kyrgyztan" "Kyrgyzstan") $
            map (replace " and " " & ") $
            map (takeWhile (/= '\t')) unAllL
        res = S.toList $ S.fromList unAllL' `S.difference` S.fromList un1L'
    putStr $ unlines res
