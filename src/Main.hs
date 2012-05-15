#include <h>

--module Main where

data LineType = Header | Num | Word | Letter
  deriving (Eq, Show)

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

runTests testResPairs = head $ map snd (filter fst testResPairs)

lineGetType :: String -> LineType
lineGetType cs =
  runTests [
    (all (\ c -> isDigit c || c == ',') cs, Num),
    (any (`isPrefixOf` cs) headerPrefixes, Header),
    (length cs == 1 && all isUpper cs, Letter),
    (True, Word)
    ]

summ :: String -> String
summ s = 
  if length s <= half * 2 
    then s 
    else take (half - 1) s ++ ".." ++ drop (length s - half + 1) s
  where
  half = 80

toChunk lineType cs = 
  if null rem then Nothing else Just (snd $ head rem, tail rem) 
  where
  rem = dropWhile ((/= lineType) . fst) cs

getFirstPage cs = do
  (countries, cs2) <- toChunk Word cs
  (cities, cs3) <- toChunk Word cs2
  (pops, cs4) <- toChunk Num cs3
  (yrs, cs5) <- toChunk Num cs4
  (pop2sArea2s, cs6) <- toChunk Num cs5
  (dens2s, cs7) <- toChunk Num cs6
  (areas, cs8) <- toChunk Num cs7
  (denssYr2s, cs9) <- toChunk Num cs8
  let denss = map head $ splitEvery 2 denssYr2s
  r <- getPage cs9
  return $ zip3 countries cities pops ++ r

getPage cs = do
  (cc, cs2) <- toChunk Word cs
  let (countries:cities:_) = splitEvery (length cc `div` 2) cc
  (lol, cs3) <- toChunk Num cs2
  let [pops, yrs, pop2s, area2s, dens2s, areas, denss, yr2s] = 
        splitEvery (length lol `div` 8) lol
      r = case getPage cs3 of
        Nothing -> []
        Just r2 -> r2
  return (zip3 countries cities pops ++ r)

countryIsUN c =
  not $ any (`isInfixOf` c) 
  [":", "Germany-", "France-", "Switzerland &", "Austria &", "Bermuda"]

n2cp :: (String, String, String) -> (String, (Int, String))
n2cp (n, c, p) = (n, (read $ filter isDigit p, c))

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
  ds = show c
    
showN :: Int -> String
showN x = 
  c ++ suf
  where
  (c, suf) = metricSigDigs 2 (fromIntegral x) 
  
showLol :: (String, (Int, String)) -> String
showLol (n, (p, c)) = showN p ++ " " ++ n ++ ": " ++ c

main :: IO ()
main = do
  ls <- lines <$> readFile "data/raw_pdf_copy"
  let 
    typeChunks =
      map (\ xs -> (fst (head xs), map snd xs)) $ 
      groupBy ((==) `on` fst) $
      map (\ l -> (lineGetType l, l)) ls    
  {- analysis phase:
  putStr $ unlines $ map summ $
    map (\ (a, b) -> [head $ show a] ++ show (length b) ++ " " ++ 
                        intercalate " " b)
    typeChunks
 -- w(country) w(city) n(pop) n(yr) n(pop2,area2) n(dens2) n(area) n(dens,yr2)
 -- w(country)w(city) n(pop)n(yr)n(pop2)n(area2)n(dens2)n(area)n(dens)n(yr2)
  -}
  writeFile "out.un1" $ unlines $
    map showLol $
    nubBy ((==) `on` fst) $
    filter (countryIsUN . fst) $
    map (\ (n, c, p) -> (n, (read $ filter isDigit p, c))) $
    fromJust $
    getFirstPage typeChunks
  {-
  putStr $ unlines $
    map (\ (n, (p, c)) -> showN p ++ " " ++ n ++ ": " ++ c) $ 
    --filter ((== "United States") . fst) $
    --filter ((== "Jakarta") . fst) $
    map (\ (n, c, p) -> (n, (read $ filter isDigit p, c))) $
    fromJust $
    getFirstPage typeChunks
  -}