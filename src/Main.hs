import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Ord
import System.Environment
import System.FilePath

data LineType = Header | Num | Word | Letter
  deriving (Eq, Show)

-- Note "nation" is a UN nation and "region" is a top-level division
-- (states, provinces, etc.).  Region also includes independently
-- administered countries which in the nation as far as the UN is concerned
-- (e.g. Taiwan in China).
data CityLoc
    = CityLoc
    { clNation :: String
    , clNationExtra :: String
    , clRegion :: String
    , clRegionExtra :: String
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
  (cities, cs3) <- toChunk Word cs2
  (pops, cs4) <- toChunk Num cs3
  (_yrs, cs5) <- toChunk Num cs4
  (_pop2sArea2s, cs6) <- toChunk Num cs5
  (_dens2s, cs7) <- toChunk Num cs6
  (_areas, cs8) <- toChunk Num cs7
  (denssYr2s, cs9) <- toChunk Num cs8
  let _denss = map head $ chunksOf 2 denssYr2s
  r <- getRemainingPages cs9
  return $ zip3 countries cities pops ++ r

getRemainingPages :: [(LineType, [String])] -> Maybe [(String, String, String)]
getRemainingPages cs = do
  (cc, cs2) <- toChunk Word cs
  let (countries:cities:_) = chunksOf (length cc `div` 2) cc
  (lol, cs3) <- toChunk Num cs2
  let [pops, _yrs, _pop2s, _area2s, _dens2s, _areas, _denss, _yr2s] =
        chunksOf (length lol `div` 8) lol
      r = case getRemainingPages cs3 of
        Nothing -> []
        Just r2 -> r2
  return (zip3 countries cities pops ++ r)

unIfy :: String -> String
unIfy "Austria & Germany" = "Austria"
unIfy "Bermuda" = "UK"
unIfy "China: Taiwan" = "China"
unIfy "France-Belgium" = "France"
unIfy "France: Mayotte" = "France"
unIfy "Germany-France" = "Germany"
unIfy "Germany-Netherlands" = "Germany"
unIfy "Greenland" = "Denmark"
unIfy "Mauretania" = "Mauritania"
unIfy "Palestine" = "Israel"
unIfy "Switzerland & D & F" = "Switzerland"
unIfy "Switzerland & F" = "Switzerland"
unIfy "US: American Samoa" = "USA"
unIfy "US: Guam" = "USA"
unIfy "US: N. Marianas" = "USA"
unIfy "US: Puerto Rico" = "USA"
unIfy "US:Virgin Islands" = "USA"
unIfy "Western Sahara" = "Morocco"
unIfy x = x

-- I have a certain set of names and abbrs. that I tend to stick too.
-- Also some other normalization and cleanup.
cleanData :: (String, String, String) -> Maybe (String, String, String)
cleanData (n, c, p) = if cityIsWack c then Nothing else Just (f n', f c', p)
  where
  (n', c') = case (n, c) of
    -- typos in data
    ("Chad", "Niamey") -> (n, "N'Djamena")
    ("Australia", "Port Maquarie") -> (n, "Port Macquarie")
    _ -> (n, c)
  f x = case x of
    "United States" -> "USA"
    "Viet Nam" -> "Vietnam"
    "Congo (Dem. Rep.)" -> "DRCongo"
    "United Kingdom" -> "UK"
    "Ivory Coast" -> "Côte d'Ivoire"
    "United Arab Emirates" -> "UAE"
    "Congo (Rep.)" -> "RCongo"
    "Dijibouti" -> "Djibouti"
    "Central African Rep." -> "CAR"
    "Serbia-Montenegro" -> "Montenegro"
    "Katowice-Gliwice-Tychy" -> "Katowice"
    "Southamption" -> "Southampton"
    _ -> x
  cityIsWack = (== "Bandaburg, QLD")  -- dupe typo for Bundaberg (AU)

expNot :: Double -> (Double, Int)
expNot x =
  (c, e)
  where
  e = floor (logBase 10 x)
  c = x / (10 ** fromIntegral e)

-- result of (1234, 2) means 1.234e2
expNotSigDigs :: Int -> Double -> (Int, Int)
expNotSigDigs d x = (cSig', e')
  where
  (cSig', e') =
    if length (show cSig) > d
      then (cSig `div` 10, e + 1)
      else (cSig, e)
  cSig = round $ c * (10 ** fromIntegral (d - 1))
  (c, e) = expNot x

placeDecimal :: Int -> Int -> String
placeDecimal pos x =
  l ++ (if null r then "" else "." ++ r)
  where
  l = take pos (sx ++ repeat '0')
  r = drop pos sx
  sx = show x

metricSigDigs :: Int -> Double -> (String, String)
metricSigDigs d x =
  if e >= 6
    then (placeDecimal (e + 1 - 6) c, "M")
    else
      if e >= 3
        then (placeDecimal (e + 1 - 3) c, "k")
        else (placeDecimal (e + 1) c, "")
  where
  (c, e) = expNotSigDigs d x

showN :: Int -> String
showN x =
  c ++ suf
  where
  (c, suf) = metricSigDigs 2 (fromIntegral x)

showLol :: (Maybe String, (Int, String)) -> String
showLol (n, (p, c)) = showN p ++ " " ++ (maybe "" (++ ": ") n) ++ c

main :: IO ()
main = do
    ls <- lines <$> readFile "data/raw_pdf_copy"
    args <- getArgs
    let typeChunks =
            map (\ xs -> (fst (head xs), map snd xs)) .
            groupBy ((==) `on` fst) $
            map (\ l -> (lineGetType l, l)) ls
        usageErr = error "Program was invoked with invalid arguments."
        runTypeArg = case args of
            [] -> "all"
            [x] -> x
            _ -> usageErr
        onePerNation = nubBy ((==) `on` fst)
        showNation = map (first Just)
        hideNation = map (first (const Nothing))
        unIfyNations = map (first unIfy)
        (runType, filterFunc, finalFunc) = case runTypeArg of
            "all" ->
                ( runTypeArg
                , const True
                , map showLol . showNation)
            "nation_by_count" ->
                ( runTypeArg
                , \ (_nation, (population, _city)) -> population >= 500000
                , map (\ (count, nation) -> show count ++ " " ++ nation) .
                  (\ xs -> xs ++ [(sum (map fst xs), "total")]) .
                  sortBy (flip (comparing fst) `mappend` comparing snd) .
                  map (\ x -> (length x, fst $ head x)) .
                  groupBy ((==) `on` fst) . sort . unIfyNations
                )
            "un1" ->
                ( runTypeArg
                , const True
                , map showLol . showNation . onePerNation . unIfyNations
                )
            n ->
                ( "by_nation" </> n
                , (== n) . unIfy . fst
                , map showLol . hideNation
                )
    writeFile ("output" </> runType) . unlines .
        finalFunc .
        filter filterFunc .
        map (\ (n, c, p) -> (n, (read $ filter isDigit p, c))) .
        mapMaybe cleanData .
        fromJust $
        getAllPages typeChunks
