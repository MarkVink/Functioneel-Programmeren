definition module Map

import BinTree		// voor het type Tree en voorbeelden t0 t/m t7
import Maybe        // voor het type Maybe

class Map m :: (a -> b) (m a) -> (m b)

instance Map []
instance Map Maybe
instance Map Tree
