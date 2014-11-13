putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn1' [] = putChar '\n'
putStrLn1' xs = putStr' xs >> putStrLn1' ""

putStrLn5' [] = putChar '\n'
putStrLn5' xs = putStr' xs >> putStr' "\n"

