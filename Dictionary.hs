module Dictionary (T, empty, lookup, insert) where
import Prelude hiding (lookup)
import qualified Prelude

newtype T a b = Dictionary [(a, b)] deriving (Show)

empty :: (Eq a, Ord a) => T a b
empty = Dictionary []

lookup :: (Eq a, Ord a) => a -> T a b -> Maybe b
lookup a (Dictionary dict) = Prelude.lookup a dict

insert :: (Eq a, Ord a) => (a, b) -> T a b -> T a b
insert pair (Dictionary dict)  = Dictionary (pair:dict)
