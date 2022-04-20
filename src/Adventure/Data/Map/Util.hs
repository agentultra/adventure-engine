module Adventure.Data.Map.Util where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

findInMap :: (a -> Bool) -> Map k a -> Maybe k
findInMap f = fmap fst . find (f . snd) . M.toList
