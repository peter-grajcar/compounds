module Compound
    ( Compound(..)
    ) where

import Element

data Compound = Atom            -- ^ Data constructor for monoatomic compounds, i.e. /H2/, /O3/, /He/
                    Element     -- ^ Atom element
                    Int         -- ^ Number of atoms in the compund
              | Group           -- ^ Data constructor for polyatomic compounds, i.e. /H2O/, /NaCl/
                    [Compound]  -- ^ List of compunds inside the group
                    Int         -- ^ Number of groups in the compund

instance Show Compound where
    show (Atom a n) = "(" ++ (getSymbol a) ++ (show n) ++ ")"
    show (Group cs n)
        | n == 1    = "(" ++ concat [show c | c <- cs] ++ ")" 
        | otherwise = "(" ++ concat [show c | c <- cs] ++ (show n) ++ ")" 
