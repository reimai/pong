module Utils (if', choices, split) where

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

--edx stuff

--all combinations with permutations
choices :: [a] -> [[a]]
choices list = sbs list (length list)

--combinations of up to n elements with permutations
sbs :: [a] -> Int -> [[a]]
sbs [] _ = [[]]; 
sbs _ 0 = [[]]; 
sbs (x:xs) n = concat ((fmap (comb' x) (sbs xs (n-1)))) ++ (sbs xs n) 
	where comb' a list = map (insert list a) [0..(length list)]

--insert element at specified index
insert :: [a] -> a -> Int -> [a] 
insert list a n = take n list ++ [a] ++ drop n list

--split the list in 2 non-empty lists all possible ways
split :: [a] -> [([a],[a])]
split list = map (split' list) [1..(length list)-1]
	where split' list n = (take n list, drop n list) 

split2 :: [a] -> [([a],[a])]
split2 [] = []
split2 (x:xs) = [(x : ls, rs) | (ls, rs) <- split xs]
