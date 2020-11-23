module SigDigs where

expNot :: Double -> (Double, Int)
expNot x = (x / (10 ** fromIntegral e), e) where e = floor $ logBase 10 x

-- A result of (1234, 2) means 1.234e2
expNotSigDigs :: Int -> Double -> (Int, Int)
expNotSigDigs d x = let
    cSig = round $ c * (10 ** fromIntegral (d - 1))
    (c, e) = expNot x
  in if length (show cSig) > d then (cSig `div` 10, e + 1) else (cSig, e)

placeDecimal :: Int -> Int -> String
placeDecimal pos x = l ++ (if null r then "" else "." ++ r)
  where
    l = take pos (sx ++ repeat '0')
    r = drop pos sx
    sx = show x

sciSigDigs :: Int -> Double -> (String, String)
sciSigDigs sigDigN x = (placeDecimal (e + 1 - 3 * thousands) c, thouAbbr)
  where
    (c, e) = expNotSigDigs sigDigN x
    thousands = e `quot` 3
    thouAbbr = case thousands of
      0 -> ""
      1 -> "k"
      2 -> "M"
      3 -> "B"
      4 -> "T"
      5 -> "Q"
      _ -> error "sciSigDigs: Should we handle beyond quintillions?"

-- XXX: Probably should deal with 0 deeper in the logic?
showN :: Int -> String
showN 0 = "0"
showN x = c ++ suf where (c, suf) = sciSigDigs 2 $ fromIntegral x
