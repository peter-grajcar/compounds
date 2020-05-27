{-|
Module: FormulaParser
Description: Module for chemical formulae parsing
-}
module FormulaParser
    ( parse
    ) where

import Compound
import Element


data Token = EmptyToken | LParenToken | RParenToken | AtomToken String | NumberToken Int

instance Show Token where
    show EmptyToken = ""
    show LParenToken = "LParen"
    show RParenToken = "RParen"
    show (AtomToken a) = "Atom(" ++ a ++ ")"
    show (NumberToken n) = "Number(" ++ (show n) ++ ")"

isNumberToken :: Char -> Bool
isNumberToken c = elem c ['0'..'9']

isUpperLetter :: Char -> Bool
isUpperLetter c = elem c ['A'..'Z']

isLowerLetter :: Char -> Bool
isLowerLetter c = elem c ['a'..'z']

-- >>> map charToInt "0123456789"
-- [0,1,2,3,4,5,6,7,8,9]
--
charToInt :: Char -> Int
charToInt c = elemIndex 0 c ['0'..'9']
    where
        elemIndex :: Eq a => Int -> a -> [a] -> Int
        elemIndex _ _ [] = error "not a NumberToken"
        elemIndex i y (x:xs)
            | x == y    = i
            | otherwise = elemIndex (i+1) y xs


-- >>> tokenize "Na(OH)2"
-- [Atom(Na),LParen,Atom(O),Atom(H),RParen,Number(2)]
--
-- >>> tokenize "H2SO4"
-- [Atom(H),Number(2),Atom(S),Atom(O),Number(4)]
--
tokenize :: [Char] -> [Token]
tokenize input = tail (process EmptyToken input)
    where
        process :: Token -> [Char] -> [Token]
        process currentToken [] = [currentToken]
        process (NumberToken n) (x:xs)
            | isNumberToken x                       = process (NumberToken (n*10 + (charToInt x))) xs
        process (AtomToken [a]) (x:xs)
            | isUpperLetter a && isLowerLetter x    = process (AtomToken ([a, x])) xs 
            | isUpperLetter x                       = (AtomToken [a]):process (AtomToken [x]) xs
        process currentToken (x:xs)
            | isNumberToken x                       = currentToken:process (NumberToken (charToInt x)) xs
            | isUpperLetter x                       = currentToken:process (AtomToken [x]) xs
            | x == '('                              = currentToken:process (LParenToken) xs
            | x == ')'                              = currentToken:process (RParenToken) xs
            | otherwise                             = error ("unexpected char " ++ [x])

-- | Parses molecular formula
--
-- >>> parse "H2O"
-- ((H2)(O1))
--
-- >>> parse "Na(OH)2"
-- ((Na1)((O1)(H1)2))
--
-- >>> parse "H2SO4"
-- ((H2)(S1)(O4))
--
parse :: String   -- ^ Molecular formula
      -> Compound -- ^ Corresponding compound
parse formula = parseCompound [Group [] 1] $ tokenize formula
    where
        parseCompound :: [Compound] -> [Token] -> Compound
        parseCompound (c:cs) []
            | null cs           = c
            | otherwise         = error "too many elements on the stack"
        parseCompound ((Group gs n):cs) ((AtomToken sym):(NumberToken an):ts) = parseCompound ((Group (gs ++ [Atom (getElementBySymbol sym) an]) n):cs) ts
        parseCompound ((Group gs n):cs) ((AtomToken sym):ts)                  = parseCompound ((Group (gs ++ [Atom (getElementBySymbol sym) 1]) n):cs) ts
        parseCompound cs (LParenToken:ts)                                     = parseCompound ((Group [] 1):cs) ts
        parseCompound ((Group g1s _):(Group g2s n2):cs) (RParenToken:(NumberToken gn):ts)    = parseCompound ((Group (g2s ++ [Group g1s gn]) n2):cs) ts
        parseCompound ((Group g1s n1):(Group g2s n2):cs) (RParenToken:ts)                    = parseCompound ((Group (g2s ++ [Group g1s n1]) n2):cs) ts

        parseCompound _ (t:_)   = error ("unexpected token " ++ (show t))
        parseCompound _ _       = error "error"
