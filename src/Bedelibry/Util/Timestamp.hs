#!/usr/bin/env runghc
--
-- timestamp.hs: A simple command line application to get a unique (short!)
-- base 85 ASCII encoded timestamp representation. Can be used to make
-- succinct, unique identifiers, as long as those identifers are generated
-- at different times.
--

import Data.List                                                                                        
import Data.Time                                                                                        
import Data.Char                                                                                        

-- Reverse function composition                                                                   
infixl 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) g f = f . g

infixl 0 #
-- Reverse function application
(#) :: a -> (a -> b) -> b
(#) a f = f $ a

-- A list of symbols to use for base 85
rads85 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()-+_|<>?:;~,.`"

-- A list of symbols to use for base 62
rads62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- A list of symbols to use for base 16
radshex = "0123456789ABCDEF"

-- Note: This function is not efficent, and will not work for large values of n.
calculateRadixes :: String -> Int -> String
calculateRadixes rad n =
-- Start with a seed (div, rem) value (the second component here is a dummy argument)
   (n, 0) # 
-- While the div is not zero, keep taking remainders. This gives us a numeric
-- representation of the digits in the base 85 representation of our number
   (unfoldr (\(div, rem) -> 
                if (div /= 0) 
                then (Just (div `mod` (length rad), div `divMod` (length rad)))
                else Nothing)) #
-- Put the numeric representation of the digits in conventional order.
   reverse #
-- Replace each numeric digit representation with a symbol from rads85 in order.
   ((map (\i -> rad !! i)) :: [Int] -> String)
                                                                                                        
getUniqTimestamp :: String -> IO String                                                                           
getUniqTimestamp rad =
-- Get the current time as a UTC value                                                                  
     getCurrentTime >>=                      
-- Get the string representation of the UTC timestamp
     (show
-- Get just the numeric part of the timestamp
      >>> (filter isNumber)
-- Covert it into an integer
      >>> (read :: String -> Int)
-- Call our helper function we defined above
      >>> calculateRadixes rad
      >>> return)

main = do
  timeStamp <- getUniqTimestamp rads62
  putStrLn timeStamp
