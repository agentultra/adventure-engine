module Adventure.List.Utils where

import Data.List

-- | Add an element to the head of a list. Push the rest of the
-- elements down and pop off the last so that the output list is the
-- same length as the input.
prepend :: a -> [a] -> [a]
prepend x [] = [x]
prepend x xs = (:) x . take (length xs - 1) $ xs
