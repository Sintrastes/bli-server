
import Crypto.Hash
import Data.ByteString.UTF8
import Data.List

-- Reverse function composition                                                                   
infixl 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) g f = f . g

infixl 0 #
-- Reverse function application
(#) :: a -> (a -> b) -> b
(#) a f = f $ a

rads62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

rads85 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()-+_|<>?:;~,.`"

calculateRadixes :: String -> Integer -> String
calculateRadixes rad n =
-- Start with a seed (div, rem) value (the second component here is a dummy argument)
   (n, 0) # 
-- While the div is not zero, keep taking remainders. This gives us a numeric
-- representation of the digits in the base 85 representation of our number
   (unfoldr (\(div, rem) -> 
                if (div /= 0) 
                then (Just (div `mod` (fromIntegral $ Data.List.length rad), div `divMod` (fromIntegral $ Data.List.length rad)))
                else Nothing)) #
-- Put the numeric representation of the digits in conventional order.
   reverse #
-- Replace each numeric digit representation with a symbol from rads85 in order.
   ((map (\i -> rad !! (fromIntegral i :: Int))) :: [Integer] -> String)

help "0" = 0
help "1" = 1
help "2" = 2
help "3" = 3
help "4" = 4
help "5" = 5
help "6" = 6
help "7" = 7
help "8" = 8
help "9" = 9
help "a" = 10
help "b" = 11
help "c" = 12
help "d" = 13
help "e" = 14
help "f" = 15

hexToInt s =                                                                                           
   sum $  
   zipWith (*)                                                                                         
      (map help                                                                                        
           (map (\x -> [x])   
               (reverse s)))   
      [16^n | n <- [0..(Prelude.length s)]]

hexTo62 s = calculateRadixes rads62 (hexToInt s)
hexTo85 s = calculateRadixes rads85 (hexToInt s)

myHash s = hexTo62 $ show (hash (fromString s) :: MD5)
myHash85 s = hexTo85 $ show (hash (fromString s) :: MD5)