
-- Some scratchwork playing around with Haskell's Data.Map

module MyMaps where

import Data.Map
import Data.List.Split
import Control.Monad

data Request = Foo | Bar deriving(Eq, Enum)

parseRequest "foo" = Just Foo
parseRequest "bar" = Just Bar
parseRequest _     = Nothing

instance Ord Request where
  compare a b = compare (fromEnum a) (fromEnum b)

myEnumMap = fromList [(Foo, 5), (Bar, 15)]

myStringMap = fromList [("Foo",5), ("Bar", 15)]

toTupMaybe :: [a] -> Maybe (a, a)
toTupMaybe (x:y:xs) = Just (x, y)
toTupMaybe xs = Nothing

maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Converts a raw URI argument string to
-- a map from argument values to string values

toArgumentMap :: String -> Map String String
toArgumentMap x =
  fromList $ join
       $ Prelude.map maybeToList
       $ Prelude.map toTupMaybe
       $ Prelude.map (splitOn "=")
       $ splitOneOf "?&" x

toRequestMap x = Data.Map.mapKeys parseRequest $ toArgumentMap x

-- Question: How do we convert from Map a b to Map (f a) (f b)? In other words,
-- how can we apply fmap to the keys and the values of a map object respectively.
-- This is useful, for example, if we want to convert from the raw string format
-- to some datatype such as Request.

-- For this purpose, we have several functions at our disposal:
--
-- map -- which maps over all of the values in the Map
-- mapWithKey -- which maps to modify the values in the Map, but taking into account also
-- the key
-- mapKeys -- which maps the keys.

