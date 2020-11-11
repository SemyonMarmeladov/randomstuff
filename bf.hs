module BrainFuck where
data Instruction = Forward
                | Backward
                | Increment
                | Decrement
                | Output
                | Input
                | Loop [Instruction]
                deriving (Show)

-- Two lists represents the memory
-- The left list is in reverse order 
-- Going forward is will take the HEAD from the right list
-- and add it the left list. going backwards will take the head
-- from the right and add it to the left
data Mem = Mem [Int] [Int] deriving (Show)

emptyMem :: Mem
emptyMem = Mem [] []
      
goForward :: Mem -> Mem
goForward (Mem y (x : xs)) = Mem (x : y) xs
goForward (Mem y []) = Mem y [0]
goForward (Mem y (x : [])) = Mem (x : y) [0]
-- [1 2 3] [0] last line should handle this case.. 
goBackward :: Mem -> Mem
goBackward (Mem (y : ys) x) = Mem ys (y : x)
goBackward (Mem [] x) = Mem [] (0 : x)


