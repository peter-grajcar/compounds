module Element
where

data Element = Element {
  getSymbol :: String,
  getName :: String, 
  getLatinName :: String
}

-- | list of all elements
elements :: [Element]
elements = [
    Element "H" "hydrogen" "hydrogenium", 
    Element "He" "helium" "helium", 
    Element "S" "sulfur" "sulphur", 
    Element "Na" "sodium" "natrium", 
    Element "Fe" "iron" "ferrum", 
    Element "O" "oxygen" "oxygenium", 
    Element "Ca" "calcium" "calcium", 
    Element "P" "phosphorus" "phosphorus", 
    Element "C" "carbon" "carbo", 
    Element "Cl" "chlorine" "chlorum"
  ]

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