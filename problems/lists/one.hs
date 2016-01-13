-- Problem 1:
-- Find the last element of a list. 

--solution 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

--solution 2 (obvious one)
myLast' :: [a] -> a
myLast' xs = last xs

--solution 3
myLast'' :: [a] -> a
myLast'' = head . reverse

--solution 4
myLast''' :: [a] -> a
myLast''' xs = xs !! (length xs - 1)
