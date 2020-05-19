module Main where

import FormulaParser
import Nomenclature

main :: IO ()
main = do
    formula <- getLine
    let compound = parse formula
    putStrLn $ compoundName compound
