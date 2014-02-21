rep :: Int -> Int -> [Int]
rep 1 x = x : []
rep n x = x : (rep (n-1) x)
          
f :: Int -> [Int] -> [Int]
f n [] = []
f n (x:xs) = (rep n x) ++ (f n xs)

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main = do
   n <- readLn :: IO Int
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f n $ map (read :: String -> Int) $ lines inputdata