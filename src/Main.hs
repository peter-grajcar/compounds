{-|
Module: Main
Description: Main module
-}
module Main where

import FormulaParser
import Nomenclature
import System.IO

-- | Reads molecular formula from the standard input and prints a compositional name.
--
-- >>> main
-- Enter Formula: CO2
-- carbon dioxide
main :: IO ()
main = do
    putStr "Enter Formula: "
    hFlush stdout
    formula <- getLine
    let compound = parse formula
    putStrLn $ compoundName compound
