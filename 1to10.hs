main :: IO ()
main = undefined

-- | http://wiki.haskell.org/99_questions/1_to_10

-- | Problem 1
-- | Find the last element of a list.

myLast :: [a] -> Maybe a
myLast [x] = Just x
myLast (_:xs) = myLast xs
myLast [] = Nothing

-- | Problem 2
-- | Find the last-but-one (or second-last) element of a list.

butLast :: [a] -> Maybe a
butLast (x:[_]) = Just x
butLast (_:xs) = butLast xs
butLast _ = Nothing

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:_) 1 = Just x
elementAt (_:xs) i = elementAt xs (i-1)

myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc+1) 0

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs
