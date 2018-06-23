transF = unlines 
  . concatMap (reverse . words) 
  . lines

main = interact transF
