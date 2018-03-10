module Max where
---CALL []
---CALL [1, 2, 3]
---CALL [4]
maxNum :: [Integer] -> Integer
---HIDE
maxNum [] = error "empty list"
maxNum (x:[]) = x
maxNum (x:xs) = if x > largest then x else largest
  where largest = maxNum xs
---EDIT
