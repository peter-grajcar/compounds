{-# LANGUAGE RecordWildCards #-}
{-|
Module: Nomenclature
Description: Module for compositional name generation from @Compound@

== Nomenclature of Inorganic Chemistry (IUPAC, 2005)
Link to the <https://iupac.org/wp-content/uploads/2016/07/Red_Book_2005.pdf full text>, <https://www.iupac.org/cms/wp-content/uploads/2016/07/Inorganic-Brief-Guide-V1-1.pdf brief guide>

Selected relevant sections from the text:

=== IR-5.2 STOICHIOMETRIC NAMES OF ELEMENTS AND BINARY COMPOUNDS
When constructing a stoichiometric name for a binary compound, one element is
designated as the electropositive constituent and the other the electronegative constituent.
The electropositive constituent is by convention the element that occurs last in the sequence
of Table VI\* and its name is the unmodified element name (Table I). The name of the
electronegative constituent is constructed by modifying the element name with the ending
‘ide’, as explained in detail for monoatomic anions in Section IR-5.3.3.2. All element names
thus modified with the ‘ide’ ending are given in Table IX.
The stoichiometric name of the compound is then formed by combining the name of the
electropositive constituent, cited first, with that of the electronegative constituent, both
suitably qualified by any necessary multiplicative prefixes (‘mono’, ‘di’, ‘tri’, ‘tetra’, ‘penta’,
etc., given in Table IV). The multiplicative prefixes precede the names they multiply, and are
joined directly to them without spaces or hyphens. The final vowels of multiplicative prefixes
should not be elided (although ‘monoxide’, rather than ‘monooxide’, is an allowed exception
because of general usage). The two parts of the name are separated by a space in English.

/Examples:/

1. HCl hydrogen chloride
2. NO nitrogen oxide, or nitrogen monooxide, or nitrogen monoxide
3. NO2 nitrogen dioxide
4. N2O4 dinitrogen tetraoxide
5. OCl2 oxygen dichloride
6. O2Cl dioxygen chloride
7. Fe3O4 triiron tetraoxide
8. SiC silicon carbide

=== IR-5.3.3.2 Monoatomic anions

The name ofamonoatomic anion is the element name (Table I) modified so as to carry the
anion designator ‘ide’, either formed by replacing the ending of the element name (‘en’,
‘ese’, ‘ic’, ‘ine’, ‘ium’, ‘ogen’, ‘on’, ‘orus’, ‘um’, ‘ur’, ‘y’ or ‘ygen’) by ‘ide’ or by directly
adding ‘ide’ as an ending to the element name.

In one case, an abbreviated name has to be chosen: germanium, germide. The systematic
name ‘germanide’ designates the anion GeH3.
Some names of monoatomic anions are based on the root of the Latin element names.
In these the ending ‘um’ or ‘ium’ is replaced by ‘ide’.

/Examples:/

8. silver, argentum, argentide
9. gold, aurum, auride
10. copper, cuprum, cupride
11. iron, ferrum, ferride
12. lead, plumbum, plumbide
13. tin, stannum, stannide

-}
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


-- >>> prependPrefix "di" "oxide"
-- "dioxide"
--
-- >>> prependPrefix "tetra" "oxide"
-- "tetroxide"
--

prependPrefix :: String -> String -> String
prependPrefix p s
    | null p                                = s
    | (last p) == 'a' && isVowel (head s)  = (init p) ++ s
    | otherwise                             = p ++ s
    where
        isVowel c = elem c ['a', 'e', 'o', 'u']


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

