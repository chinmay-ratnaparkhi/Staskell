data Stack = Empty | Full Double Stack --deriving (Show)


-- The insert function accepts a double, a stack and returns a new stack
-- with the value added to the top of the stack.
insert :: Double -> Stack -> Stack
insert x y = Full x y

-- The peek function takes a stack and returns the value that's at the 
-- top of the stack.
peek :: Stack -> Maybe Double
peek Empty = Nothing
peek (Full x _) = Just x

-- The pop function takes a stack and returns a new stack after eleminating
-- the element at the top.
pop :: Stack -> Stack
pop Empty = Empty
pop (Full _ y) = y

-- The inserter function takes a list and builds a stack with everything in
-- the list
inserter :: [Double] -> Stack
inserter xs =  foldl (\acc a -> (insert a acc)) Empty xs

instance Show Stack where
	show Empty = "Empty \n--------------"
	show (Full x y) = (show x) ++ "\n" ++ show y