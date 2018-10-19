module Roman where

solution :: String -> Int
solution "" = 0
solution [x] =
  case x of
    'I' -> 1
    'V' -> 5
    'X' -> 10
    'L' -> 50
    'C' -> 100
    'D' -> 500
    'M' -> 1000
solution (x:y:xs) =
  case [x,y] of
    "IV" -> 4 + r
    "IX" -> 9 + r
    "XL" -> 40 + r
    "XC" -> 90 + r
    "CD" -> 400 + r
    "CM" -> 900 + r
    _    -> solution [x] + solution (y:xs)
  where r = solution xs
solution (x:xs) = solution [x] + solution xs
