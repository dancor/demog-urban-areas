module SigDigs where

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
