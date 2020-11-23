import Control.Applicative
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Ord
import System.Directory
import System.Environment
import System.FilePath

import SigDigs

data LineType = Header | Num | Word | Letter
  deriving (Eq, Show)

-- "Nation" is a UN nation and "region" is a top-level division
-- (states, provinces, etc.).  Region also includes independently
-- administered countries in the nation for UN purposes
-- (e.g. Taiwan in China).
-- The "extra" parts record additional places that a city may spill
-- over into.
data CityLoc
    = CityLoc
    { clNation :: String
    , clNationExtra :: String
    , clRegion :: String
    , clRegionExtra :: String
    , clCity :: String
    }
    deriving (Eq, Ord, Show)

data CityInfo
    = CityInfo
    { ciLoc :: CityLoc
    , ciPop :: Int
    }
    deriving (Eq, Ord, Show)

showCi :: Bool -> CityInfo -> String
showCi doFullNation (CityInfo (CityLoc n nX r rX c) p) =
    showN p ++ " " ++
    (if null nFull then "" else nFull ++ ": ") ++ c ++
    (if null rFull then "" else ", " ++ rFull)
  where
    rFull = r ++ rX
    nFull = if doFullNation then n ++ nX else n

nationFix :: String -> (String, String, String)
nationFix "Austria & Germany" = ("Austria", "-Germany", "")
nationFix "Bermuda" = ("UK", "", "Bermuda")
nationFix "China: Taiwan" = ("China", "", "Taiwan")
nationFix "France-Belgium" = ("France", "-Belgium", "")
nationFix "France: Mayotte" = ("France", "", "Mayotte")
nationFix "Germany-France" = ("Germany", "-France", "")
nationFix "Germany-Netherlands" = ("Germany", "-Netherlands", "")
nationFix "Greenland" = ("Denmark", "", "Greenland")
nationFix "Katowice-Gliwice-Tychy" = ("Katowice", "-Gliwice-Tychy", "")
nationFix "Palestine" = ("Israel", "", "Palestine")
-- Here's an unusual case where the nominal nation was put second:
nationFix "Serbia-Montenegro" = ("Montenegro", "-Serbia", "")
nationFix "Switzerland & D & F" = ("Switzerland", "-France-Germany", "")
nationFix "Switzerland & F" = ("Switzerland", "-France", "")
nationFix "US: American Samoa" = ("USA", "", "American Samoa")
nationFix "US: Guam" = ("USA", "", "Guam")
nationFix "US: N. Marianas" = ("USA", "", "N. Marianas")
nationFix "US: Puerto Rico" = ("USA", "", "Puerto Rico")
nationFix "US:Virgin Islands" = ("USA", "", "Virgin Islands")
nationFix "Western Sahara" = ("Morocco", "", "Western Sahara")
nationFix x = (x, "", "")

cityFix :: String -> (String, String, String)
cityFix "Chaoyang-Chaonan (Shantou,) GD" =
    cityFix "Chaoyang-Chaonan (Shantou), GD"
cityFix "Puning (Jieyang,) GD" =
    cityFix "Puning (Jieyang), GD"
cityFix "Yuyao (Ningbo)" =
    cityFix "Yuyao (Ningbo), ZJ"
cityFix "Huangyan (Taizhou) ZJ" =
    cityFix "Huangyan (Taizhou), ZJ"
cityFix "Zhuji, (Shaoxing), ZJ" =
    cityFix "Zhuji (Shaoxing), ZJ"
cityFix "Shangyu, (Shaoxing), ZJ" =
    cityFix "Shangyu (Shaoxing), ZJ"
cityFix "Zhangjiaggang-Jiangyin, (Suzhou-Wuxi) JS" =
    cityFix "Zhangjiaggang-Jiangyin (Suzhou-Wuxi), JS"
cityFix "Vadodara. GUJ" = cityFix " Vadodara, GUJ"
cityFix "Visakhpatnam.AP" = cityFix "Visakhpatnam, AP"
cityFix "Vijayawada. AP" = cityFix "Vijayawada, AP"
cityFix "Varanasi.UP" = cityFix "Varanasi, UP"
cityFix "Jabalpur.MP" = cityFix "Jabalpur, MP"
cityFix "Warangal. AP" = cityFix "Warangal, AP"
cityFix "Jamnagar.GUJ" = cityFix "Jamnagar, GUJ"
cityFix "Rajamundry" = cityFix "Rajamundry, AP"
cityFix "Shreveport LA" = cityFix "Shreveport, LA"
cityFix "" = cityFix ""
cityFix cityOrig =
    if null commaAndRest
    then ("", "", cityOrig)
    else (region, regionExtra, city)
  where
    (city, commaAndRest) = break (== ',') cityOrig
    regionFull = dropWhile isSpace $ drop 1 commaAndRest
    (region, regionExtra) = break (== '-') regionFull

cleanLoc :: String -> String -> CityLoc
-- Typos in data:
cleanLoc "Chad" "Niamey" = cleanLoc "Chad" "N'Djamena"
cleanLoc "Australia" "Port Maquarie" = cleanLoc "Australia" "Port Macquarie"
cleanLoc n c = CityLoc nation nationExtra region regionExtra city
  where
    (nation, nationExtra, region1) = nationFix n
    (region2, regionExtra, city) = cityFix c
    region =
        if null region1
        then region2
        else region1

cleanData :: (String, String, String) -> Maybe CityInfo
-- Dupe typo for Bundaberg (AU):
cleanData (_n, "Bandaburg, QLD", _p) = Nothing
cleanData (n, c, p) =
    Just $
    CityInfo (cleanLoc (f n) (noDubDash $ f c)) (read $ filter isDigit p)
  where
    -- Simple replacements (typos, abbrs.):
    f "Central African Rep." = "CAR"
    f "Congo (Dem. Rep.)" = "DRCongo"
    f "Congo (Rep.)" = "RCongo"
    f "Dijibouti" = "Djibouti"
    f "Ivory Coast" = "Côte d'Ivoire"
    f "Mauretania" = "Mauritania"
    f "Southamption" = "Southampton"
    f "United Arab Emirates" = "UAE"
    f "United Kingdom" = "UK"
    f "United States" = "USA"
    f "Viet Nam" = "Vietnam"
    f x = x
    -- Kill inconsistent use of double dashes:
    noDubDash ('-':'-':xs) = noDubDash ('-':xs)
    noDubDash (x:xs) = x : noDubDash xs
    noDubDash [] = []

tryTakePrefix :: String -> String -> Maybe (String, String)
tryTakePrefix prefix s =
    if prefix `isPrefixOf` s then Just (prefix, drop (length prefix) s)
                             else Nothing

tryTakePrefixes :: [String] -> String -> Maybe (String, String)
tryTakePrefixes prefixes s =
    listToMaybe . catMaybes $ map (\ p -> tryTakePrefix p s) prefixes

readDemogLine :: String -> (String, String, String)
readDemogLine l = (nation, city, population)
  where
    [nation, city, population, _area] = splitOn "\t" l
{-
    (placeName, stats) = break isDigit l
    city = reverse . dropWhile isSpace . reverse $ dropWhile isSpace spCity
    (nation, spCity) = fromMaybe (p1, unwords pRest) $ tryTakePrefixes
        [ "Congo (Dem. Rep.)"
        , "Congo (Rep.)"
        , "Ivory Coast"
        , "Saudi Arabia"
        , "South Africa"
        , "South Korea"
        , "United Kingdom"
        , "United States"
        , "Viet Nam"
        ]
        placeName
    p1:pRest = words placeName
    population = head . words $ filter (/= ',') stats
-}

readDemog :: String -> [CityInfo]
readDemog = mapMaybe cleanData . map readDemogLine . lines

main :: IO ()
main = do
    cityInfos <- readDemog <$> readFile "data/ua-tsv.txt"
    args <- getArgs
    let dataHasAllCitiesThisSize = 500000
        usageErr = error "Program was invoked with invalid arguments."
        runTypeArg = case args of
            [] -> "all"
            [x] -> x
            _ -> usageErr
        compNation = (==) `on` clNation . ciLoc
        hideNation (CityInfo (CityLoc _n _nX r rX c) p) =
            CityInfo (CityLoc "" "" r rX c) p
        (outSubDir, outFile, filterFunc, finalFunc) = case runTypeArg of
            "all" ->
                ( "", runTypeArg
                , const True
                , map (showCi True)
                )
            "nation-by-count" ->
                ( "", runTypeArg
                , (>= dataHasAllCitiesThisSize) . ciPop
                , map (\ (count, nation) -> show count ++ " " ++ nation) .
                  (\ xs -> xs ++ [(sum (map fst xs), "total")]) .
                  sortBy (flip (comparing fst) `mappend` comparing snd) .
                  map (\ x -> (length x, clNation . ciLoc $ head x)) .
                  groupBy compNation . sort
                )
            "un1" ->
                ( "", runTypeArg
                , const True
                , map (showCi False) . nubBy compNation
                )
            'R':':':n ->
                ( "by-region", n
                , (== n) . clNation . ciLoc
                , map (showCi True . hideNation) .
                  sortBy (compare `on` (clRegion . ciLoc))
                )
            n ->
                ( "by-nation", n
                , (== n) . clNation . ciLoc
                , map (showCi False . hideNation)
                )
    createDirectoryIfMissing True $ "output" </> outSubDir
    writeFile ("output" </> outSubDir </> outFile) . unlines .
        finalFunc $
        filter filterFunc cityInfos
