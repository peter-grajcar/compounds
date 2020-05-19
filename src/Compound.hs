module Compound
    ( Compound(..)
    ) where

import Element

data Compound = Atom Element Int     -- ^ Data constructor for monoatomic compounds, i.e. H2, O3, He
              | Group [Compound] Int -- ^ Data constructor for polyatomic compounds, i.e. H2O, NaCl

instance Show Compound where
    show (Atom a n) = "(" ++ (getSymbol a) ++ (show n) ++ ")"
    show (Group cs n)
        | n == 1    = "(" ++ concat [show c | c <- cs] ++ ")" 
        | otherwise = "(" ++ concat [show c | c <- cs] ++ (show n) ++ ")" 
