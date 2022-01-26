--Programming with Lists and Bags

-- Creating special bag type

type Bag a = [(a,Int)]

-- Examples:

es1 :: Bag Int
lst_es1 = [2,3,3,5,7,7,7,8]
es1 = [(5,1),(7,3),(2,1),(3,2),(8,1)]

es2 :: Bag Int
lst_es2 = [7,3,8,7,3,2,7,5]
es2 = [(5,1),(7,3),(2,1),(3,2),(8,1)]

es3 :: Bag Int
lst_es3 = [5,7,2,3,8]
es3 = [(5,1),(7,1),(2,1),(3,1),(8,1)]


-- Inserting integer to bag

ins :: Eq a => a -> Bag a -> Bag a
ins a [] = [(a, 1)]
ins a ((x,n):xs)
           |a == x = ((x, succ(n)) : xs)
           |otherwise = ((x,n) : ins a xs) 

-- Deliting specific integer from bag

del :: Eq a  => a -> Bag a -> Bag a
del a [] = []
del a (x:xs)
        | a == fst x = ((a, pred(snd x)) : xs)
        | otherwise  = x : (del a xs)

-- Creating bag based on specific list

bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

-- Checking whether bag contains all elements from other bag

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] [] = True
subbag [] ys = True
subbag xs [] = False
subbag (x:xs) ys
           | elem x ys = subbag xs ys
           | otherwise = False

-- Checking whether bag is actuall a set

isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet (x:xs)
          | snd x == 1 = isSet xs
          | otherwise = False

-- Checking a size of the bag

size :: Bag a -> Int
size [] = 0
size (x:xs) = snd x + size xs

