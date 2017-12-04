data Deque a = Deque [a] [a] deriving (Show)

-- Начало первого списка - левый конец очереди.
-- Конец первого списка ~ середина очереди слева.
-- Начало второго списка - правый конец очереди.
-- Конец второго списка ~ середина очереди справа.

-- Таким образом append и appendLeft всегда O(1), т.к. (:) - O(1).
-- pop и popLeft почти всегда O(1) по тем же причинам, кроме случая,
-- когда один из списков опустеет, а в другом будет больше одного элемента.

append :: Deque a -> a -> Deque a
append (Deque in_ out) v = Deque in_ (v:out)


appendLeft :: Deque a -> a -> Deque a
appendLeft (Deque in_ out) v = Deque (v:in_) out


halve :: [a] -> ([a], [a])
halve lst = splitAt (length lst `div` 2) lst

equalize :: Deque a -> Deque a
equalize (Deque in_ []) = Deque (fst half) (reverse $ snd half) where
  half = halve in_
equalize (Deque [] out) = Deque (reverse $ snd half) (fst half) where
  half = halve out


pop :: Deque a -> (Deque a, a)
pop (Deque [] []) = error "Deque is empty"
pop (Deque in_ (h:t)) = (Deque in_ t, h)
-- Случаи, когда правый список опустеет
pop (Deque (h:[]) []) = (Deque [] [], h)
pop deque@(Deque in_ []) = pop $ equalize deque

popLeft :: Deque a -> (Deque a, a)
popLeft (Deque [] []) = error "Deque is empty"
popLeft (Deque (h:t) out) = (Deque t out, h)
-- Случаи, когда левый список опустеет
popLeft (Deque [] (h:[])) = (Deque [] [], h)
popLeft deque@(Deque [] out) = popLeft $ equalize deque

-- Метод банкира:
--   Зарабатываем 1$ за каждое добавление.
--   При удалении из деки может возникнуть случай, когда нужный список опустел.
--   Тогда нужно производить "перекидывание".
--   Перекидываем в этом случае N/2, где N мы уже заработали на добавлениях.
--   Соответственно, амортизированная сложность всех операций над декой - константа.
--
-- Метод физика:
--   Потенциал - сумма длин списков.
--   Добавление увеличивает потенциал на 1.
--   Удаление без "перекидывания" не меняет потенциал.
--   Удаление с "перекидыванием" уменьшает потенциал на N и занимает O(N),
--   но случается не чаще чем каждую N/2 операцию.
