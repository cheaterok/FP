{-
Реализуйте структуру данных "бинарное дерево поиска" для целых чисел без балансировки. Реализация включает функции:

  Добавления элемента: insert :: BinaryTree -> Integer -> BinaryTree
  Удаления элемента: remove :: BinaryTree -> Integer -> BinaryTree
  Создания пустого дерева: emptyTree :: BinaryTree
  Поиска элемента в дереве: containsElement :: BinaryTree -> Integer -> Bool
  Поиска в дереве наименьшего элемента, который больше или равен заданному: nearestGE :: BinaryTree -> Integer -> Integer
  Создания дерева из списка: treeFromList :: [Integer] -> BinaryTree
  Создания списка из дерева: listFromTree :: BinaryTree -> [Integer]

Операторы insert и remove должны поддерживать цепочки вызовов в инфиксной форме:
  listFromTree (emptyTree `insert` 1 `insert` 2 `insert` 3) === [1,2,3]
-}

data BinaryTree = EmptyTree
                  | Node {value::Integer, left::BinaryTree, right::BinaryTree}


insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree new_val = Node new_val EmptyTree EmptyTree
insert (Node v l r) c =
  if c < v
    then Node v (insert l c) r
  else
    Node v l (insert r c)


emptyTree :: BinaryTree
emptyTree = EmptyTree


containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Node v l r) c
  | v == c = True
  | v < c = containsElement l v
  | v > c = containsElement r v


-- Поиск в дереве наименьшего элемента, который больше или равен заданному
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree _ = error "Подходящее значение не найдено"
nearestGE (Node v l r) c
  | v == c = v -- Если равны - нашли
  -- Если значение левой ветки всё ещё больше нужного числа - идти по левой ветке
  | v > c = if value l > c then nearestGE l c else v
  -- Если значение меньше - идти по правой ветке
  | v < c = nearestGE r c


leftMostElement :: BinaryTree -> BinaryTree
leftMostElement EmptyTree = EmptyTree
leftMostElement node@(Node _ EmptyTree _) = node
leftMostElement (Node _ l _ ) = leftMostElement l


-- https://ru.wikipedia.org/wiki/Двоичное_дерево_поиска
remove :: BinaryTree -> Integer -> BinaryTree
-- Если дерево пустое - остановится
remove EmptyTree _ = EmptyTree
-- Иначе сравнить K с ключом X корневого узла
remove (Node x l r) k
  -- Если K>X, циклически удалить k из правого поддерева
  | k > x = Node x l (remove r k)
  -- Если K<X, циклически удалить K из левого поддерева
  | k < x = Node x (remove l k) r
  -- Если K=X, то необходимо рассмотреть три случая:
  | k == x = remove' (Node x l r)
  where
    remove' :: BinaryTree -> BinaryTree
    -- Если обоих детей нет, то удаляем текущий узел и обнуляем ссылку на него у родительского узла
    remove' (Node _ EmptyTree EmptyTree) = EmptyTree
    -- Если одного из детей нет, то значения полей ребёнка ставим вместо соответствующих значений корневого узла,
    -- затирая его старые значения, и освобождаем память, занимаемую узлом
    remove' (Node _ l EmptyTree) = l
    remove' (Node _ EmptyTree r) = r
    -- Если оба ребёнка присутствуют, то:
    remove' (Node _ l r)
    -- Если левый узел правого поддерева отсутствует (n->right->left),
    -- то копируем из правого узла в удаляемый поля K, V и ссылку на правый узел правого потомка
      | EmptyTree <- (left r) = Node (value r) l (right r)
    -- Иначе возьмём самый левый узел правого поддерева n->right
    -- cкопируем данные (кроме ссылок на дочерние элементы) из m в n;
    -- рекурсивно удалим узел m
      | otherwise = Node (value lmax) l (remove r $ value lmax)
      where lmax = leftMostElement r


treeFromList :: [Integer] -> BinaryTree
treeFromList lst = foldl insert EmptyTree lst


-- Список из дерева = значение наименьшего элемента : список из дерева (без этого элемента)
listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []
listFromTree tree = let
  leftmost_value = value $ leftMostElement tree
  tree_wo_leftmost_value = remove tree leftmost_value in
  leftmost_value : listFromTree tree_wo_leftmost_value
