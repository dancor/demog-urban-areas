{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Safe

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
procLastPage = map (\(c:p:n:k:_) -> Entry p c (rInt n) (rInt k)) .
    take lastN . chunksOf 7 . drop lastN

main = do
    p:ps <- splitWhen ("\12" `T.isPrefixOf`) . takeWhile (/= "\12Table 3") . 
        drop 5 . dropWhile (/= "\12Table 2") . T.lines <$> 
        T.readFile "db-worldua.txt"
    T.writeFile "data.csv" . T.unlines . ("Population\tkm2\tCountry\tCity":) .
        map (\(Entry p c n k) -> T.pack (show n) <> "\t" <> T.pack (show k) <>
        "\t" <> c <> "\t" <> p) $ procPage0 p ++ 
        concatMap procMidPage (init ps) ++ procLastPage (last ps)
