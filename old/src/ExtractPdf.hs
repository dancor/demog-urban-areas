{-# LANGUAGE BangPatterns #-}

import Data.Char
import Data.List

main :: IO ()
main = interact $ unlines . map procLine . grabTable1Content . lines

grabTable1Content :: [String] -> [String]
grabTable1Content =
    takeWhile (not . ("Table 2" `isInfixOf`)) .
    filter (not . ("Demographia" `isInfixOf`)) .
    filter (not . null) .
    drop 1 . dropWhile (not . (== ["Rank"]) . take 1 . words) .
    drop 1 . dropWhile (not . ("Table 1" `isInfixOf`)) .
    drop 1 . dropWhile (not . ("Table 1" `isInfixOf`))

procLine :: String -> String
procLine = intercalate "\t" .
    (\x ->
        [x !! 0, x !! 1, filter (/= ',') $ x !! 2, filter (/= ',') $ x !! 7]
    ) .
    multiSpaceSplit . dropWhile (\c -> isSpace c || isDigit c)

multiSpaceSplit :: String -> [String]
multiSpaceSplit = multiSpaceSplitAccum ""

multiSpaceSplitAccum :: String -> String -> [String]
multiSpaceSplitAccum !accum (' ':' ':xs) =
    accum : multiSpaceSplitAccum "" (dropWhile (== ' ') xs)
multiSpaceSplitAccum !accum (x:xs) = multiSpaceSplitAccum (accum ++ [x]) xs
multiSpaceSplitAccum !accum [] = []
