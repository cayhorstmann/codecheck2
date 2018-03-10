maxNum :: [Integer] -> Integer
---HIDE
maxNum [] = error "empty list"
maxNum (x:[]) = x
maxNum (x:xs) = if x > largest then x else largest
  where largest = maxNum xs
---EDIT

main :: IO ()
main = do
  print $ maxNum [1,2,3]
  print $ maxNum [4]
  print $ maxNum []
