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

headerPrefixes :: [String]
headerPrefixes = [
  "Demographia World Urban Areas: ",
  "Table 1",
  "LARGEST URBAN AREAS IN THE WORLD",
  "Threshold Population for Ranking",
  "Rank",
  "Geography",
  "Urban Area",
  "Population",
  "Estimate",
  "Year",
  "Base Year Land Area:",
  "Square",
  "Miles",
  "Land",
  "Area:",
  "Km2",
  "Density Base Year",
  "Popula-",
  "tion",
  "Method",
  "Area",
  "Source"
  ]

multiIf :: [(Bool, a)] -> a
multiIf = snd . head . filter fst

lineGetType :: String -> LineType
lineGetType cs =
  multiIf [
    (all (\ c -> isDigit c || c == ',') cs, Num),
    (any (`isPrefixOf` cs) headerPrefixes, Header),
    (length cs == 1 && all isUpper cs, Letter),
    (True, Word)
    ]

toChunk
    :: LineType
    -> [(LineType, [String])]
    -> Maybe ([String], [(LineType, [String])])
toChunk lineType cs =
  if null cs2 then Nothing else Just (snd $ head cs2, tail cs2)
  where
  cs2 = dropWhile ((/= lineType) . fst) cs

getAllPages :: [(LineType, [String])] -> Maybe [(String, String, String)]
getAllPages cs = do
  (countries, cs2) <- toChunk Word cs
  (urbAreas, cs3) <- toChunk Word cs2
  (pops, cs4) <- toChunk Num cs3
  (_yrs, cs5) <- toChunk Num cs4
  (_pop2sArea2s, cs6) <- toChunk Num cs5
  (_dens2s, cs7) <- toChunk Num cs6
  (_areas, cs8) <- toChunk Num cs7
  (denssYr2s, cs9) <- toChunk Num cs8
  let _denss = map head $ chunksOf 2 denssYr2s
  r <- getRemainingPages cs9
  return $ zip3 countries urbAreas pops ++ r

getRemainingPages :: [(LineType, [String])] -> Maybe [(String, String, String)]
getRemainingPages cs = do
  (cc, cs2) <- toChunk Word cs
  let (countries:urbAreas:_) = chunksOf (length cc `div` 2) cc
  (lol, cs3) <- toChunk Num cs2
  let [pops, _yrs, _pop2s, _area2s, _dens2s, _areas, _denss, _yr2s] =
        chunksOf (length lol `div` 8) lol
      r = case getRemainingPages cs3 of
        Nothing -> []
        Just r2 -> r2
  return (zip3 countries urbAreas pops ++ r)

showCi :: CityInfo -> String
showCi (CityInfo (CityLoc n nX r rX c) p) =
    showN p ++ " " ++
    (if null nFull then "" else nFull ++ ": ") ++ c ++
    (if null rFull then "" else ", " ++ rFull)
  where
    rFull = r ++ rX
    nFull = n ++ nX

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
    f "Dijibouti" = "Djibouti"
    f "Mauretania" = "Mauritania"
    f "United States" = "USA"
    f "Viet Nam" = "Vietnam"
    f "Congo (Dem. Rep.)" = "DRCongo"
    f "United Kingdom" = "UK"
    f "Ivory Coast" = "Côte d'Ivoire"
    f "United Arab Emirates" = "UAE"
    f "Congo (Rep.)" = "RCongo"
    f "Central African Rep." = "CAR"
    f "Southamption" = "Southampton"
    f x = x
    -- Kill inconsistent use of double dashes:
    noDubDash ('-':'-':xs) = noDubDash ('-':xs)
    noDubDash (x:xs) = x : noDubDash xs
    noDubDash [] = []

readDemog :: String -> [CityInfo]
readDemog inp =
    mapMaybe cleanData .
    fromJust $
    getAllPages linesChunkedByType
  where
    linesChunkedByType =
        map (\ xs -> (fst (head xs), map snd xs)) .
        groupBy ((==) `on` fst) $
        map (\ l -> (lineGetType l, l)) $ lines inp

main :: IO ()
main = do
    cityInfos <- readDemog <$> readFile "data/raw_pdf_copy"
    args <- getArgs
    let dataHasAllCitiesThisSize = 500000
        usageErr = error "Program was invoked with invalid arguments."
        runTypeArg = case args of
            [] -> "all"
            [x] -> x
            _ -> usageErr
        compNation = (==) `on` (clNation . ciLoc)
        hideNation (CityInfo (CityLoc _n _nX r rX c) p) =
            CityInfo (CityLoc "" "" r rX c) p
        (outSubDir, outFile, filterFunc, finalFunc) = case runTypeArg of
            "all" ->
                ( "", runTypeArg
                , const True
                , map showCi
                )
            "nation_by_count" ->
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
                , map showCi . nubBy compNation
                )
            'R':':':n ->
                ( "by_region", n
                , (== n) . clNation . ciLoc
                , map (showCi . hideNation) .
                  sortBy (compare `on` (clRegion . ciLoc))
                )
            n ->
                ( "by_nation", n
                , (== n) . clNation . ciLoc
                , map (showCi . hideNation)
                )
    createDirectoryIfMissing True $ "output" </> outSubDir
    writeFile ("output" </> outSubDir </> outFile) . unlines .
        finalFunc $
        filter filterFunc cityInfos
