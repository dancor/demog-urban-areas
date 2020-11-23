module SigDigs where

expNot :: Double -> (Double, Int)
expNot x = (floor $ logBase 10 x, x / (10 ** fromIntegral e))

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
placeDecimal pos x = l ++ (if null r then "" else "." ++ r)
  where
    l = take pos (sx ++ repeat '0')
    r = drop pos sx
    sx = show x

sciSigDigs :: Int -> Double -> (String, String)
sciSigDigs d x = (placeDecimal (e + 1 - 3 * thousands) c, thouAbbr)
  where
    (c, e) = expNotSigDigs d x
    thousands = e `quot` 3
    thouAbbr = case thousands of
      0 -> ""
      3 -> "k"
      6 -> "M"
      9 -> "B"
      12 -> "T"
      15 -> "Q"
      _ -> error "sciSigDigs: Should we handle beyond quintillions?"

-- XXX: Probably should deal with 0 deeper in the logic?
showN :: Int -> String
showN 0 = "0"
showN x = c ++ suf where (c, suf) = sciSigDigs 2 $ fromIntegral x
