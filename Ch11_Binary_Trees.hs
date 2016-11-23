-- Definition of binary tree structure
data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- insert node function    
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
    | b == a = Node left a right 
    | b < a  = Node (insert' b left) a right 
    | b > a  = Node left a (insert' b right)
t1 = insert' 1 Leaf
t2 = insert' 3 t1
t3 = insert' 5 t2

-- mapTree function
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf -- Base case
mapTree f (Node left a right) = -- Recursive case
    Node (mapTree f left) (f a) (mapTree f right)
mappedt3 = mapTree (\x -> 2 * x) t3

-- Convert binary trees to lists
preorder :: BinaryTree a -> [a]
preorder = 