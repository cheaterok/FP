import Prelude hiding ((!!))

data DList a = DEmpty | DCons (DList a) a (DList a)

instance (Eq a) => Eq (DList a) where
  DEmpty == DEmpty = True
  (DCons _ lh lt) == (DCons _ rh rt) = (lh == rh) && (lt == rt)

instance (Show a) => Show (DList a) where
  show lst = "[" ++ show' lst ++ "]"
    where
      show' DEmpty = ""
      show' (DCons _ h DEmpty) = show h
      show' (DCons _ h t) = show h ++ ", " ++ show' t

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DEmpty
list2dlist' left (h:t) = r where
  r = DCons left h (list2dlist' r t)

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DEmpty lst



insert :: DList a -> a -> Int -> DList a
insert _ _ v | v < 0 = error "Negative index"
-- Пустой список
insert DEmpty item 0 = DCons DEmpty item DEmpty
insert DEmpty item n = error "List is too small"
-- Середина списка
insert (DCons l v r) item 0 = new_node where
  new_node = DCons l item right -- Вставляем вместо значения
  right = DCons new_node v r -- То что было смещаем вправо
-- Идём по списку
insert (DCons l v r) item n = DCons l v $ insert r item (n-1)


delete :: Eq a => Show a => DList a -> a -> DList a
delete DEmpty _ = DEmpty
delete (DCons l v r@(DCons ll vv rr)) c
  | v == c = DCons l vv rr
  | otherwise = DCons l v $ delete r c
delete (DCons l v DEmpty) c = DEmpty

(!!) :: DList a -> Int -> a
_ !! v | v < 0 = error "Negative index"
DEmpty !! n = error "List is too small"
(DCons l v r) !! 0 = v
(DCons l v r) !! n = r !! (n-1)
