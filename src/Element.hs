module Element
where

import System.IO
import System.IO.Unsafe
import Data.Char

data Element = Element {
  getSymbol :: String,
  getName :: String, 
  getLatinName :: String
} deriving (Show)


-- >>> splitOn ',' "abcd,123,efgh,345"
-- ["abcd","123","efgh","345"]
--
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim list = split [] list
  where
    split w [] = [w]
    split w (x:xs)
      | x == delim  = w:split [] xs
      | otherwise   = split (w ++ [x]) xs
  

-- >>> do elems <- loadElements; return (getName . head $ elems)
-- "hydrogen"
--
loadElements :: IO [Element]
loadElements = do
  table <- readFile "res/periodic-table.csv"
  let rows = tail . lines $ table
  return (map (makeElement . splitOn ',') rows)
  where
    getLines h = hGetContents h >>= return . lines
    makeElement cols = Element (cols!!3) (map (toLower) $ cols!!1) (map (toLower) $ cols!!2)


-- | List of all elements. The list is loaded from @res/periodicTable.csv@.
elements :: [Element]
elements = unsafePerformIO loadElements

-- | Returns element with matching symbol. If no matching element is found, error is raised.
--
-- >>> getElementBySymbol "He"
-- ("He","helium","helium")
--
getElementBySymbol :: String  -- ^ sybmol
                   -> Element -- ^ corresponding element
getElementBySymbol symbol = search symbol elements
    where
        search :: String -> [Element] -> Element
        search sym (e:es)
            | sym == (getSymbol e)  = e
            | otherwise                 = search sym es
        search sym [] = error ("element " ++ sym ++ " not found")