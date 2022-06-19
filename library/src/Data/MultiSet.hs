module Data.MultiSet where

-- https://hackage.haskell.org/package/heaps
-- https://hackage.haskell.org/package/heaps-0.3.6.1/docs/Data-Heap.html

import           Data.Heap                      ( Heap )
import qualified Data.Heap                     as Heap
import           Data.Ord

maximum :: Heap (Down a) -> Down a
maximum = Heap.minimum

deleteMax :: Heap (Down a) -> Heap (Down a)
deleteMax = Heap.deleteMin

viewMax :: Heap (Down a) -> Maybe ((Down a), Heap (Down a))
viewMax = Heap.viewMin
