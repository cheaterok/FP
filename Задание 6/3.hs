data Stream a = Cons a (Stream a) deriving (Show)

-- Brothers' Formulae
brothersE 0 = 2
brothersE n = brothersE (n-1) + (2*n + 2) / (product [1..(2*n + 1)])

ePrecisions :: Stream Rational
ePrecisions = r 0 where
  r n = Cons (brothersE n) (r (n+1))
