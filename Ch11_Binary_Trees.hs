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

-- Create a small tree
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
-- preorder :: BinaryTree a -> [a]
preorder bt = 
    case bt of
        Leaf           -> []
        Node btl v btr -> [v] ++ preorder btl ++ preorder btr
inorder bt = 
    case bt of
        Leaf           -> []
        Node btl v btr -> inorder btl ++ [v] ++ inorder btr
postorder bt = 
    case bt of
        Leaf           -> []
        Node btl v btr -> postorder btl ++ postorder btr ++ [v]
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
testPreoder :: IO()
testPreoder =
    if preorder testTree == [2,1,3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."
testInorder :: IO()
testInorder =
    if inorder testTree == [1,2,3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."
testPostorder :: IO()
testPostorder =
    if postorder testTree == [1,3,2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

-- foldTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x bt =
    foldr f x lst
    where
        lst = inorder bt

-- mapTree based on foldTree
-- mapTree' :: (a -> b)
--         -> BinaryTree a
--         -> BinaryTree b
-- mapTree' f bt =
--     foldTree z z z 


