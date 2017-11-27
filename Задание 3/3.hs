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


newtype PSet'' a = PSet''{ contains'' :: (a -> Bool) }
-- Симметричная разность множеств a.k.a XOR
instance Monoid (PSet'' a) where
  mempty = PSet'' (\x -> False)
  mappend (PSet'' x) (PSet'' y) = PSet'' (\c -> (x c && (not $ y c)) || ((not $ x c) && y c))


-- Для fmap нужна функция (PSet a -> PSet b), тип PSet определяется типом *a* в contains :: (a -> Bool)
-- Следовательно, чтобы сделать функтор, нам нужна функция (a -> b), но мы ничего не знаем об a.

instance Functor PSet where
  fmap _ _ = PSet (\_ -> False)

instance Functor PSet' where
  fmap _ _ = PSet' (\_ -> True)
