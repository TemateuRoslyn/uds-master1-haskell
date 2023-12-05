-- ############  Exercice : Proposons une Structure de doneees liste chainee polymorphe



-- 1>>> Defintition du type

-- data List a = Nil | Cons a (List a)
data List a = Nil | Cons {head_ :: a, resi:: List a} deriving Show

-- 1.1>>> liste d'exemple
lc = Cons 12 (Cons 100 (Cons 299 (Cons 19 (Cons 1000 (Cons 1 (Nil))))))
lc1 = Nil
-- 2.1>>> Element Maximun ( version 1)

-- maxElm :: (Ord a) => List a -> Maybe a
-- maxElm Nil = Nothing
-- maxElm (Cons x Nil) = Just x
-- maxElm (Cons x (Cons y resi)) = if x > y then maxElm (Cons x resi)
--                                 else maxElm (Cons y resi)

-- 2.2>>> Element Maximun ( version 2)

maxElm :: (Ord a) => List a -> Maybe a
maxElm Nil = Nothing
maxElm Cons { head_ = x, resi = Nil } = Just x
maxElm Cons { head_ = x, resi =  
                         Cons {head_ = y, resi = rest}
                         } = if x > y then maxElm (Cons x rest)
                                else maxElm (Cons y rest)

-- 3>>> Taille de la liste chainee

tailMax :: List a -> Int
tailMax Nil = 0
tailMax ( Cons x y ) = 1 + tailMax y

-- 4>>> Ajout en tete d'une liste chainee

ajoutT :: a -> List a -> List a
ajoutT x Nil = (Cons x Nil)
ajoutT z liste = Cons z liste 

-- 5>>> supprimer en tete

delHead :: List a -> List a
delHead Nil = Nil 
delHead (Cons x reste) = reste

-- 6>>> return elm post i
-- le 1er element est a l'indice 0

getPostI :: Int -> List a -> Maybe a
getPostI _ Nil = Nothing
getPostI i (Cons x rest) = if i== 0 then Just x 
                        else getPostI  (i-1) rest

-- 7>>> supprimer elm post i
delPostI :: Int -> List a -> List a
delPostI i Nil = Nil
delPostI i (Cons x rest) = if i==0 then rest
                            else Cons x (delPostI (i-1) rest)

-- 8>>> add elm en queue
addEnd :: a -> List a -> List a 
addEnd x Nil = (Cons x Nil)
addEnd x (Cons y rest ) = Cons y (addEnd x rest) 

-- 9>>> delete tail elm
delElmTail :: List a -> List a
delElmTail Nil = Nil
delElmTail (Cons x Nil) = Nil
delElmTail (Cons y rest) = Cons y (delElmTail  rest) 

-- 10>>> add elm pos i
addPostI :: a -> Int -> List a ->List a
addPostI x _ Nil = (Cons x Nil) 
addPostI x i (Cons y rest) = if i==0 then (Cons x (Cons y rest))
                                        else (Cons y (addPostI x (i-1) rest))

  

