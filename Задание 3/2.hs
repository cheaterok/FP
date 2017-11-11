data ReverseList a = RNil | RCons (ReverseList a) a

toList RNil = []
toList (RCons xs x) = x : toList xs

toRList = reverse.toList


instance (Show a) => Show (ReverseList a) where
    show = show.toList

instance (Eq a) => Eq (ReverseList a) where
    a == b = toList a == toList b

instance (Ord a) => Ord (ReverseList a) where
    a <= b = toList a <= toList b

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend x RNil = x
    mappend RNil y = y
    mappend x (RCons y v) = RCons (mappend x y) v

instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons xs x) = RCons (fmap f xs) (f x)
