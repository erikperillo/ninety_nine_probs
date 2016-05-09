{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -The first function has been completed as an example. All the other functions are undefined.
 -They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
penultimate :: [a] -> a
penultimate xs = (last . init) xs

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK :: Int -> [a] -> a
findK k xs = xs !! k

findK' :: Int -> [a] -> a
findK' _ [] = error "u habin a giggle m8?"
findK' 0 (x:xs) = x
findK' k (x:xs) = findK' (k-1) xs

-- Determine if list l is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [x] = True
isPalindrome' (x:xs) = (x == last xs) && (isPalindrome' (init xs))

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:(duplicate xs)

zipList :: [a] -> [a] -> [[a]]
zipList _ [] = []
zipList [] _ = []
zipList (x:xs) (y:ys) = [x,y]:(zipList xs ys)

duplicate' :: [a] -> [a]
duplicate' xs = concat (zipList xs xs)

duplicate'' :: [a] -> [a]
duplicate'' xs = concat (zipWith (\x y -> [x,y]) xs xs)

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}

zipLike :: [a] -> [b] -> [(a,b)]
zipLike _ [] = []
zipLike [] _ = []
zipLike (x:xs) (y:ys) = (x,y):(zipLike xs ys)

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex :: Int -> [a] -> ([a],[a])
splitAtIndex 0 xs = ([],xs)
splitAtIndex k (x:xs) = 
    let 
        (left,right) = splitAtIndex (k-1) xs
    in
        (x:left,right) 
splitAtIndex _ [] = error "u WOOT M8???????"

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK :: Int -> [a] -> [a]
dropK 0 (x:xs) = xs
dropK k (x:xs) = x:(dropK (k-1) xs)
dropK _ [] = error "eyb0ss"

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice :: Int -> Int -> [a] -> [a]
slice i k xs = 
    if i < k then 
        (xs !! i):(slice (i+1) k xs)
    else
        []

slice' :: Int -> Int -> [a] -> [a]
slice' i k xs
    | k > length xs || i < 0 || i > k = []
    | i < k = (xs !! i):(slice' (i+1) k xs)
    | otherwise = []

slice'' :: Int -> Int -> [a] -> Maybe [a]
slice'' _ _ [] = Just []
slice'' i k xs
    | k > length xs || i < 0 || i > k = Nothing
    | otherwise = Just ((take (k-i) . drop i) xs)

slice''' :: Int -> Int -> [a] -> [a]
slice''' _ _ [] = []
slice''' i k xs
    | k > length xs || i < 0 || i > k = []
    | otherwise = reverse $ sliceAcc (k-i) (drop i xs) []
        where
            sliceAcc 0 _ acc = acc
            sliceAcc n (x:xs) acc = sliceAcc (n-1) xs (x:acc)

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: a -> Int -> [a] -> [a]
insertElem x k xs = (take k xs) ++ [x] ++ (drop k xs)

insertElem' :: a -> Int -> [a] -> [a]
insertElem' x k xs = insertElemAcc x k [] xs
    where
        insertElemAcc :: a -> Int -> [a] -> [a] -> [a]
        insertElemAcc y 0 acc xs = (reverse acc) ++ [y] ++ xs
        insertElemAcc y k acc (x:xs) = insertElemAcc y (k-1) (x:acc) xs

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = (drop k xs) ++ (take k xs)
    where
        k = mod n (length xs)

rotate' :: Int -> [a] -> [a]
rotate' _ [] = []
rotate' 0 xs = xs
rotate' k (x:xs) = rotate' (k-1) (xs ++ [x])
