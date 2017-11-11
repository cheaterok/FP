data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber) | Pred (WeirdPeanoNumber)

-- Тайпклассы Integer'а
-- instance Eq Integer
-- instance Ord Integer
-- instance Show Integer
-- instance Read Integer
-- instance Enum Integer
-- instance Num Integer
-- instance Real Integer
-- instance Integral Integer


-- Если переводить WeirdPeanoNumber в число (по условию сгодится и Integer)
-- то можно использовать уже определённые операторы Integer'а
toInt :: WeirdPeanoNumber -> Integer
toInt Zero = 0
toInt (Succ a) = (toInt a) + 1
toInt (Pred a) = (toInt a) - 1

fromInt :: Integer -> WeirdPeanoNumber
fromInt x | x > 0 = Succ $ fromInt (x - 1)
          | x < 0 = Pred $ fromInt (x + 1)
          | otherwise = Zero


instance Eq WeirdPeanoNumber where
    (==) a b = (toInt a) == (toInt b)

instance Ord WeirdPeanoNumber where
    (<=) a b = (toInt a) <= (toInt b)

instance Show WeirdPeanoNumber where
    show = show.toInt

-- Read чёт потно

instance Enum WeirdPeanoNumber where
    toEnum = fromIntegral -- Возьмёт из Num
    fromEnum = fromInteger.toInt

instance Num WeirdPeanoNumber where
    a + b = fromInt $ (toInt a) + (toInt b)
    a * b = fromInt $ (toInt a) * (toInt b)
    abs = fromInt.abs.toInt
    signum = fromInt.signum.toInt
    negate = fromInt.negate.toInt
    fromInteger = fromInt

instance Real WeirdPeanoNumber where
    toRational = toRational.toInt

instance Integral WeirdPeanoNumber where
    toInteger = toInt
    -- quotRem :: Integral a => a -> Rational
    -- Возвращает пару (результат деления, остаток от деления)
    quotRem x y = (fromInt a, fromInt b)
             where (a, b) = quotRem (toInt x) (toInt y)

-- 25
test = show $ abs $ Succ (-101) `div` Pred 5
