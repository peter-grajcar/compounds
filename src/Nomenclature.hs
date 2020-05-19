{-# LANGUAGE RecordWildCards #-}
module Nomenclature
    ( compoundName
    ) where

import Compound
import Element
import Data.List

-- | list of element suffixes from section IR-5.3.3.2
elementSuffixes :: [String]
elementSuffixes = ["ogen", "ygen", "orus", "ese", "ine", "ium", "ic", "en", "on", "um", "ur", "y"]

-- | list of simple multiplicative prefixes
simpleMultPrefixes :: [String]
simpleMultPrefixes = ["", "di", "tri", "tetra", "penta", "hexa", "hepta", "octa", "nona", "deca", "undeca", "dodeca"]

-- | list of complicated multiplicative prefixes
complicatedMultPrefixes :: [String]
complicatedMultPrefixes = ["", "bis", "tris", "tetrakis", "pentakis", "hexakis", "heptakis", "octakis", "nonakis", "decakis", "undecakis", "dodecakis"]

-- | Returns stem prefix of element name
-- >>> getStem "oxygen"
-- "ox"
--
-- >>> getStem "sodium"
-- "sod"
--
getStem :: String -> String
getStem name = take (findSuffixIndex name elementSuffixes) name
    where
        findSuffixIndex :: String -> [String] -> Int
        findSuffixIndex name (e:es) = if i /= -1 then i else findSuffixIndex name es
            where i = indexOf e (suffixes name)
        findSuffixIndex name [] = length name

        suffixes :: [a] -> [[a]]
        suffixes []         = [] : []
        suffixes f@(_:t) = f:suffixes t

-- | Returns anion name for given @Atom@.
--
-- "Exceptions include Zn and Group 18 elements ending in 'on', where the 'ide' ending is added to the element names", 
-- "For some elements (e.g. Fe, Ag, Au) a Latin stem is used before the 'ide' ending"
-- >>> anionName $ Atom (getElementBySymbol "S") 2
-- "disulfide"
--
-- >>> anionName $ Atom (getElementBySymbol "Fe") 1
-- "ferride"
--
anionName :: Compound -> String
anionName (Atom e n) = prependPrefix (simpleMultPrefixes !! (n - 1)) (ide e)
    where 
        ide :: Element -> String
        ide Element{..}
            | getName == "Zn"   = "zincide"
            | getName == "Ne"   = "neonide"
            | getName == "Ar"   = "argonide"
            | getName == "Kr"   = "kryptonide"
            | getName == "Xn"   = "xenonide"
            | getName == "Rn"   = "radonide"
            | getName  == "Og"   = "oganessonide"
            | elem getName ["Fe", "Ag", "Au", "Pb", "Sn", "Cu"]    
                                        = (getStem getLatinName) ++ "ide"
            | otherwise                 = (getStem getName) ++ "ide"
anionName c = error ((show c) ++ " is not a homoatomic anion")


-- | Element sequence according to Table VI, page 260
elementSequence :: [String]
elementSequence = ["F", "Cl", "Br", "I", "At", "O", "S", "Se", "Te", "Po", "Lv", "H", "N", "P", 
                    "As", "Sb", "Bi", "C", "Si", "Ge", "Sn", "Pb", "Fl", "B", "Al", "Ga", "In", 
                    "Tl", "Zn", "Cd", "Hg", "Cn", "Cu", "Ag", "Au", "Rg", "Ni", "Pd", "Pt", "Ds",
                    "Co", "Rh", "Ir", "Mt", "Fe", "Ru", "Os", "Hs", "Mn", "Tc", "Re", "Bh", "Cr",
                    "Mo", "W", "Sg", "V", "Nb", "Ta", "Db", "Ti", "Zr", "Hf", "Rf", "Sc", "Y", 
                    "La", "Lu", "Ac", "Lr", "Be", "Mg", "Ca", "Sr", "Ba", "Ra", "Li", "Na", "K",
                    "Rb", "Cs", "Fr", "He", "Ne", "Ar", "Kr", "Xe", "Rn", "Og"]


-- | Returns index of the first occurence of element in list. If the element is not present in the list -1 is returned.
--
-- >>> indexOf 'l' "hello"
-- 2
--
indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = index 0 x xs
    where
        index :: Eq a => Int -> a -> [a] -> Int
        index _ _ [] = -1
        index n y (x:xs)
            | y == x    = n
            | otherwise = index (n+1) y xs

        
-- | Returns position in the element sequence.
--
-- See also 'elementSequence'
--
-- >>> sequencePosition $ parse (tokenize "O2")
-- 5
--
-- >>> sequencePosition $ parse (tokenize "(PO4)2")
-- 5
--
sequencePosition :: Compound -> Int
sequencePosition (Atom  e  _) = indexOf (getSymbol e) elementSequence
sequencePosition (Group cs _) = minimum (map (sequencePosition) cs)

-- | splits the compoud to electronegative and electropositive constituents.
-- >>> splitConstituents $ parse (tokenize "Ca3(PO4)2")
-- (((P1)(O4)2),[(Ca3)])
--
-- >>> splitConstituents $ parse (tokenize "CO2")
-- ((O2),[(C1)])
--
splitConstituents :: Compound -> (Compound, [Compound])
splitConstituents (Group cs _) = foldr  (\c (neg, pos) -> if (sequencePosition c) < (sequencePosition neg) 
                                                            then (c, pos ++ [neg])
                                                            else (neg, pos ++ [c]) ) 
                                        (head cs, []) 
                                        (tail cs)
splitConstituents _ = undefined


-- >>> prependPrefix "tetra" "oxide"
-- "tetroxide"
--
isVowel :: Char -> Bool
isVowel c = elem c ['a', 'e', 'i', 'o', 'u']

prependPrefix :: String -> String -> String
prependPrefix p s
    | null p                                = s
    | isVowel (last p) && isVowel (head s)  = (init p) ++ s
    | otherwise                             = p ++ s


-- | Generates compound name for given @Compound@ according to IUPAC's <https://iupac.org/wp-content/uploads/2016/07/Red_Book_2005.pdf Nomenclature of Inorganic Chemistry>.
--
-- See also 'Compound', 'parse'
--
-- >>> compoundName $ parse "CO2"
-- "carbon dioxide"
--
-- >>> compoundName $ parse "Ca3(PO4)2"
-- "tricalcium bis(phosphorus tetroxide)"
--
-- >>> compoundName $ parse "NaCl"
-- "sodium chloride"
--
-- >>> compoundName $ parse "H2SO4"
-- "dihydrogen sulfur tetroxide"
--
compoundName :: Compound -> String
compoundName (Atom e n) = prependPrefix (simpleMultPrefixes !! (n - 1)) $ getName e 
compoundName g = makeName $ splitConstituents g
    where
        makeName (Atom e n, cs)     = concatMap ((++ " ") . compoundName) cs ++ anionName    (Atom e n)
        makeName (Group gs n, cs)
            | n > 1                 = concatMap ((++ " ") . compoundName) cs ++ (complicatedMultPrefixes !! (n - 1)) ++ "(" ++ compoundName (Group gs n) ++ ")"
            | otherwise             = concatMap ((++ " ") . compoundName) cs ++ compoundName (Group gs n)

