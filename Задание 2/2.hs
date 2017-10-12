-- Обходим сначала
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Обходим с конца
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)


-- Разница в свёрточных функциях:
-- в foldl функция берёт аккумулятор и первый элемент списка, возвращает аккумулятор
-- в foldr функция берёт первый элемент списка и аккумулятор, возвращает аккумулятор (идёт с конца)


myMapL :: (a -> b) -> [a] -> [b]
myMapL f xs = reverse $ foldl (\acc x -> (f x) : acc) [] xs
myMapR :: (a -> b) -> [a] -> [b]
myMapR f xs = foldr (\x acc -> (f x) : acc) [] xs


myFlatMapL :: (a -> [b]) -> [a] -> [b]
myFlatMapL f xs = reverse $ foldl (\acc x -> (f x) ++ acc) [] xs
myFlatMapR :: (a -> [b]) -> [a] -> [b]
myFlatMapR f xs = foldr (\x acc -> (f x) ++ acc) [] xs

myConcatL :: [a] -> [a] -> [a]
myConcatL a b = foldl (\acc x -> acc ++ [x]) a b
myConcatR :: [a] -> [a] -> [a]
myConcatR a b = foldr (:) b a


myFilterL :: (a -> Bool) -> [a] -> [a]
myFilterL bool_func xs = foldl (\acc x -> if bool_func x then acc ++ [x] else acc) [] xs
myFilterR :: (a -> Bool) -> [a] -> [a]
myFilterR bool_func xs = foldr (\x acc -> if bool_func x then x : acc else acc) [] xs


myMaxByL :: (a -> Integer) -> [a] -> a
myMaxByL key_f xs = foldl (\acc x -> if key_f x > key_f acc then x else acc) (head xs) xs
myMaxByR :: (a -> Integer) -> [a] -> a
myMaxByR key_f xs = foldr (\x acc -> if key_f x > key_f acc then x else acc) (last xs) xs


myMinByL :: (a -> Integer) -> [a] -> a
myMinByL key_f xs = foldl (\acc x -> if key_f x < key_f acc then x else acc) (head xs) xs
myMinByR :: (a -> Integer) -> [a] -> a
myMinByR key_f xs = foldr (\x acc -> if key_f x < key_f acc then x else acc) (last xs) xs


myReverseL :: [a] -> [a]
myReverseL xs = foldl (\acc x -> x : acc) [] xs
myReverseR :: [a] -> [a]
myReverseR xs = foldr (\x acc -> acc ++ [x]) [] xs


myElementAtL :: Integer -> [a] -> a
myElementAtL n xs = head $ foldl (\acc _ -> tail acc) xs $ replicate (fromIntegral n) 0
myElementAtR :: Integer -> [a] -> a
myElementAtR n xs = head $ foldr (\_ acc -> tail acc) xs $ replicate (fromIntegral n) 0


myIndexOfL :: String -> [String] -> Integer
myIndexOfL s xs = foldl (\acc x -> if s == snd x then fst x else acc) (-1) (zip [0..] xs)
myIndexOfR :: String -> [String] -> Integer
myIndexOfR s xs = foldr (\x acc -> if s == snd x then fst x else acc) (-1) (zip [0..] xs)
