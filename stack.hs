push :: [Int] -> Int -> [Int]
push stack x = stack ++ [x]

top :: [Int] -> Int
top [x] = x
top (x:xs) = top xs

pop :: [Int] -> [Int]
pop [] = error "empty list"
pop (x:xs)
  | (x == (top (x:xs))) = xs
  | otherwise = x:(pop xs)

is_empty :: [Int] -> Bool
is_empty [] = True
is_empty _ = False
