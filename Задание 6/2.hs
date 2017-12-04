-- Просто так дерайвить Show на бесконечных последовательностях, конечно, опасно
-- Но если держать Ctrl+C наготове, то терминал, возможно, останется жив.
data Stream a = Cons a (Stream a) deriving (Show)


-- Формула из википедии, верна только для значений от -2pi до 2pi
-- Чтобы она была верна и для остальных Double, нужно взять x `mod` (2*pi)
maclaurinSin :: Double -> Int -> Double
maclaurinSin x 0 = x
maclaurinSin x n = maclaurinSin x (n-1) + (-1)^n * x^(2*n + 1)/(fromIntegral $ product [1..(2*n + 1)])


sinPrecisions :: Double -> Stream Double
sinPrecisions x = r x 0 where
  r x n = Cons (maclaurinSin x n) (r x (n+1))
