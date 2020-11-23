{-# LANGUAGE OverloadedStrings #-}
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Split
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Safe
import SigDigs

p0N = 54
midN = 63
lastN = 56

data Entry = Entry
  { ePlace   :: !Text
  , eCountry :: !Text
  , ePop     :: !Int
  , eKm2     :: !Int
  } deriving (Show)

rInt :: Text -> Int
rInt = readNote "lol" . T.unpack . T.filter (/= ',') 

procPage0 :: [Text] -> [Entry]
procPage0 p = zipWith4 Entry places countries (map rInt pops) km2s
  where
    (countries, p2) = splitAt p0N $ drop (p0N + 2) p
    (places, p3) = splitAt p0N $ drop 2 p2
    (pops, p4) = splitAt p0N $ drop 3 p3
    km2s = map rInt . take p0N . map (!! 1) . chunksOf 2 $ drop 11 p4

procMidPage :: [Text] -> [Entry]
procMidPage p = zipWith4 Entry places countries (map rInt pops) km2s
  where
    (countries, p2) = splitAt midN $ drop midN p
    (places, p3) = splitAt midN $ drop 1 p2
    (pops, p4) = splitAt midN $ drop 3 p3
    km2s = map rInt . take midN $ drop (1 + midN + 1) p4

procLastPage :: [Text] -> [Entry]
procLastPage = map (\(c:p:n:k:_) -> (f p c) (rInt n) (rInt k)) .
    take lastN . chunksOf 7 . drop lastN
  where
    f p c = if p == "China" || p == "United States" || p == "Russia"
      then Entry c p else Entry p c

main :: IO ()
main = do
    p:ps <- splitWhen ("\12" `T.isPrefixOf`) . takeWhile (/= "\12Table 3") . 
        drop 5 . dropWhile (/= "\12Table 2") . T.lines <$> 
        T.readFile "db-worldua.txt"
    let es = procPage0 p ++ concatMap procMidPage (init ps) ++
            procLastPage (last ps)
    T.writeFile "data.tsv" . T.unlines . ("Population\tkm2\tCountry\tCity":) $
        map (\(Entry p c n k) -> T.pack (show n) <> "\t" <> T.pack (show k) <>
        "\t" <> c <> "\t" <> p) es
    T.writeFile "all.tsv" . T.unlines $
        map (\(Entry p c n _) -> T.pack (showN n) <> "\t" <> p <> "\t" <> c) es
    T.writeFile "by-nation.tsv" . T.unlines .
        map (\((tot, pos), Entry p c n _) -> T.pack (showN n) <> "\t" <> p <> "\t" <>
        c <> " #" <> T.pack (show pos) <> "/" <> T.pack (show tot)) .
        concat . map (\l -> zip (map ((,) (length l)) [1..]) l) .
        sortBy (flip $ comparing length <> comparing (ePop . head)) .
        map snd . HM.toList . HM.fromListWith (flip (++)) $ 
        map (\e -> (eCountry e, [e])) es
