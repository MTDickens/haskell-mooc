module MinSqr where

minSqrHelper xs ys = (k,(avg ys) - k * (avg xs))
  where k = ((sum $ zipWith (*) xs ys) - n * (avg xs) * (avg ys)) / ((sum $ zipWith (*) xs xs) - n * (avg xs) * (avg xs))
        n = fromIntegral $ length xs
        avg list = (sum list) / (fromIntegral $ length list)

minSqr xs ys = case minSqrHelper xs ys of
  (k,b) -> "y = " ++ show k ++ "x + " ++ show b
