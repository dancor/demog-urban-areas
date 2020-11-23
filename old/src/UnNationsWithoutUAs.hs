import Control.Applicative
import Data.List
import Data.List.Utils
import qualified Data.Set as Set

main = do
    un1L <- lines <$> readFile "output/un1"
    unAllL <- lines <$> readFile "data/un-nations"
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
        theWith = un1L'
        theWithout = Set.toList $
            Set.fromList unAllL' `Set.difference` Set.fromList un1L'
    writeFile "output/un-nations-with-uas" $ unlines theWith
    writeFile "output/un-nations-without-uas" $ unlines theWithout
