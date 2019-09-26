{-# LANGUAGE ExistentialQuantification #-}

--
-- An example implementation, except in haskell
-- instead of Python
--

import Data.Typeable
import Data.Dynamic
import Control.Monad


-- An existential type for things that are Typeable
data Typeable' = forall a . Typeable a => MkTypeable a

pack :: Typeable a => a -> Typeable'
pack = MkTypeable

-- typeOf', but for things in our existential type
typeOf' (MkTypeable t) = typeOf t

type DynamicValue = (String,  [Typeable'])
type DynamicRecordType = (String, [TypeRep])

valToType :: DynamicValue -> DynamicRecordType
valToType (s, vals) = (s, map typeOf' vals)

x :: DynamicValue
x = ("age", [pack "nate",
             pack (25 :: Int)])

str_ref = typeOf ""
bool_ref = typeOf True
int_ref = typeOf (0 :: Int)

getSqlTypename :: TypeRep -> Maybe String
getSqlTypename rep 
  | rep == str_ref  = Just "VARCHAR" 
  | rep == bool_ref = Just "BOOLEAN"
  | rep == int_ref  = Just "INTEGER"
  | otherwise = Nothing

getSqlRowEncoding :: DynamicValue -> Maybe (String,[(String,String)])
-- returns a list of pairs (varname, sqltype) if the
-- DynamicValue contains SQL-storable types.
getSqlRowEncoding val =
  case maybeHelper of
    Just xs -> Just $ (fst val, zip (take (length xs) vars) xs)
    Nothing -> Nothing
  where typeList = snd $ valToType val
        maybeHelper :: Maybe [String]
        -- This is a useful pattern to remember
        maybeHelper = forM (map getSqlTypename typeList) (\x -> x)
        vars = map (\n -> "var"++show n) [1..]

getSqlRowValEncoding :: DynamicValue -> Maybe ([String],[String])
getSqlRowValEncoding = undefined

formatSqlCreateTable :: (String, [(String,String)]) -> String
formatSqlCreateTable (name, pairs) = 
    "CREATE TABLE " ++ name ++ "(\n" ++
      sepBy ",\n" (map (\(x,y) -> "  " ++ x ++ " " ++ y) pairs)
   ++ "\n);"
  where sepBy s xs = foldr1 (\x -> \y -> x ++ s ++ y) xs

formatSqlValues :: String -> [String] -> String
formatSqlValues table_name valueList =
     "INSERT INTO " ++ table_name ++ "(" ++ sepBy ", " (take (length valueList) vars) ++ ")"
  ++ " VALUES(" ++ sepBy ", " valueList ++ ")"  
  where sepBy s xs = foldr1 (\x -> \y -> x ++ s ++ y) xs
        vars = map (\n -> "var"++show n) [1..]
store :: DynamicValue -> String -> IO ()
-- Take a dynamic value, and store it in
-- the sqlite database with the specified path,
-- checking to see if the types match
-- the pre-existing schema before trying to insert 
store = undefined