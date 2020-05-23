module Main where

import FormulaParser
import Nomenclature
import System.IO

main :: IO ()
main = do
    putStr "Enter Formula: "
    hFlush stdout
    formula <- getLine
    let compound = parse formula
    putStrLn $ compoundName compound
