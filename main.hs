data Maybe_ a = Just_ a | Nothing_ deriving Show

divi :: Int -> Int -> Maybe_ Int
divi a 0 = Nothing_
divi a  b = Just_ (a `div` b)