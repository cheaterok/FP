data BinaryTree = EmptyTree
                  | Node { parent :: BinaryTree, value::Integer,
                           left::BinaryTree, right::BinaryTree}
                  deriving (Show)


insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree c = Node EmptyTree c EmptyTree EmptyTree
insert (Node p v EmptyTree r) c
  | c < v = parent_node where
    parent_node = Node p v new_node r
    new_node = Node parent_node c EmptyTree EmptyTree
insert (Node p v l EmptyTree) c
  | c >= v = parent_node where
    parent_node = Node p v l new_node
    new_node = Node parent_node c EmptyTree EmptyTree
insert (Node p v l r) c
  | c < v = Node p v (insert l c) r
  | otherwise = Node p v l (insert r c)


remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree
remove (Node p v l r) c | c < v = Node p v (remove l c) r
                        | c > v = Node p v l (remove r c)
                        | otherwise = merge l r
                  where merge EmptyTree t = t
                        merge (Node p v l r) t = Node p v l (merge r t)


containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Node _ v l r) c
  | v == c = True
  | v < c = containsElement l v
  | v > c = containsElement r v
