module OMap (OMap, empty, insert, OMap.lookup, values) where

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

newtype OMap k v = OMap (Map k v, [k])

empty :: Ord k => OMap k v
empty = OMap (Map.empty, [])

-- Fails if the key was already inserted.
insert :: Ord k => k -> v -> OMap k v -> OMap k v
insert k v (OMap (m, ks)) =
  let n = Map.insertWith (error "INTERNAL: Must not insert key twice") k v m in
  OMap (n, k : ks)

lookup :: Ord k => k -> OMap k v -> Maybe v
lookup k (OMap (m, _)) = Map.lookup k m

-- Returns values in the order they were inserted.
values :: Ord k => OMap k v -> [v]
values (OMap (m, ks)) =
  map (\k -> fromJust (Map.lookup k m)) (reverse ks)
