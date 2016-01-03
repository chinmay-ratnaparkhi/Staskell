data Stack = Empty | Full Double Stack deriving (Show)


-- The insert method accepts a double, a stack and returns a new stack
-- with the value added to the top of the stack.
insert :: Double -> Stack -> Stack
insert x y = Full x y

-- The inserter method takes a list and builds a stack with everything in
-- the list
inserter :: [Double] -> Stack
inserter xs =  foldl (\acc a -> (insert a acc)) Empty xs


-- The peek method takes a stack and returns the value that's at the 
-- top of the stack.
peek :: Stack -> Maybe Double
peek Empty = Nothing
peek (Full x _) = Just x
