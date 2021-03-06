{-
Реализовать на Haskell любые 3 из следующих функций:

  * Синус или косинус числа на основе ряда Тейлора или Маклорена
  * Наибольший общий делитель двух чисел
  * Существует ли натуральное число, являющееся квадратом другого числа, между двумя заданными целыми числами?
  * Является ли заданная дата (число, месяц, год) корректным числом с учётом високосных годов и количества дней в месяце?
  * Возведение целого числа в целую степень (готовую реализацию использовать, разумеется, нельзя, числа с плавающей точкой тоже, решение за линейное время не принимается)
-}

-- Косинус на основе ряда Тейлора
-- Первый аргумент функции - угол в радианах.
-- Второй аргумент функции - кол-во элементов ряда для суммирования.
-- Для точности, сравнимой с библиотечной функцией, вполне достаточно 10.
myCos :: Double -> Int -> Double
myCos x 0 = 1
myCos x n = myCos x (n-1) + (-1)^n * x^(2*n) / (fromIntegral $ product [1..2*n])

-- Наибольший общий делитель двух чисел
-- http://younglinux.info/algorithm/euclidean
(?) :: Int -> Int -> Int
a ? b
  | a < 0 || b < 0 = error "Не сойдётся"
  | a == b = a
  | a > b = (a - b) ? b
  | a < b = a ? (b - a)

-- Возведение целого числа в целую степень
-- https://www.rookieslab.com/posts/fast-power-algorithm-exponentiation-by-squaring-cpp-python-implementation
(^*^) :: Int -> Int -> Int
_ ^*^ 0 = 1
0 ^*^ _ = 0
a ^*^ b
  | even b = (a*a) ^*^ (b `div` 2)
  | odd b = a * (a ^*^ (b-1))
