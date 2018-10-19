module Roman where
-- illustration: mathsisfun.com/numbers/images/roman-1984.gif

toDecimal :: String -> Int
toDecimal "" = 0
toDecimal [x] =
  case x of
    'I' -> 1
    'V' -> 5
    'X' -> 10
    'L' -> 50
    'C' -> 100
    'D' -> 500
    'M' -> 1000
toDecimal (x:y:xs) =
  case [x,y] of
    "IV" -> 4 + r
    "IX" -> 9 + r
    "XL" -> 40 + r
    "XC" -> 90 + r
    "CD" -> 400 + r
    "CM" -> 900 + r
    _    -> toDecimal [x] + toDecimal (y:xs)
  where r = toDecimal xs
toDecimal (x:xs) = toDecimal [x] + toDecimal xs
