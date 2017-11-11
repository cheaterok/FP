newtype PSet a = PSet{ contains :: (a -> Bool) }
-- Содержится ли искомое хоть в одном из множеств
instance Monoid (PSet a) where
  mempty = PSet (\x -> False)
  mappend (PSet x) (PSet y) = PSet (\c -> x c || y c)


newtype PSet' a = PSet'{ contains' :: (a -> Bool) }
-- Содержится ли искомое в каждом из множеств
instance Monoid (PSet' a) where
  mempty = PSet' (\x -> False)
  mappend (PSet' x) (PSet' y) = PSet' (\c -> x c && y c)

-- Вроде больше ничего осмысленного не придумать


-- fmap :: Functor PSet => (a -> b) -> PSet a -> PSet b
-- PSet :: (a -> Bool) -> PSet a
-- contains :: PSet a -> a -> Bool

-- На такой солянке особо ничего не придумаешь

instance Functor PSet where
  fmap _ _ = PSet (\_ -> False)

instance Functor PSet' where
  fmap _ _ = PSet' (\_ -> True)
