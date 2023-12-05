-- manipulation des arbres
-- 1>>> Defintition du type
data Btree a = Leaf a | Node a (Btree a) (Btree a) deriving Show

maxThree :: (Ord a) => a -> a -> a -> a
maxThree x y z = max x (max y z)
ba = Node 3 (Node 6 (Node 2 (Leaf 4) (Leaf 6)) (Node 9 (Leaf 15) (Leaf 56))) (Node 8 (Leaf 22) (Leaf 506))

-- 2>>>max element
maxElm ::(Ord a)=> Btree a -> a
maxElm (Leaf x) = x
maxElm (Node x (Leaf x1) (Leaf x2)) = (maxThree x x1 x2)
maxElm (Node x (Leaf x1) d) = maxThree x x1 (maxElm d)
maxElm (Node x g (Leaf x1)) = maxThree x x1 (maxElm g)
maxElm (Node x f g) = let 
                    maxLeft = maxElm f
                    maxRight = maxElm g 
                    in maxThree x maxLeft maxRight

          