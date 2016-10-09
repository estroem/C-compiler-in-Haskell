import Data.Char
import Data.List
import Data.Maybe

data Ast = Number Integer | Name String | App Ast [Ast]
    deriving (Eq, Show, Ord)

data ExprElem = Op String | Ast Ast
    deriving (Show)

ops = ["+", "-", "*", "/", "++"]
opNumArgs = [2, 2, 2, 2, 1]
opPrecedence = [1, 1, 2, 2, 3]
opAssoc = [0, 0, 0, 0, 0]

--- TOKENIZE

tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs)
    | isDigit x = (x : takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
    | isAlpha x = (x : takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
    | x == ' ' || x == '\t' || x == '\n' = tokenize xs
    | length xs >= 2 && elem (x:take 2 xs) ops = (x : (take 2 xs)) : (tokenize (drop 2 xs))
    | length xs >= 1 && elem (x:take 1 xs) ops = (x : (take 1 xs)) : (tokenize (tail xs))
    | elem [x] ops = [x] : tokenize xs
    | otherwise = error $ "Illegal symbol \"" ++ [x] ++ "\""

--- PARSE

parseExpr :: [String] -> (Ast, [String])
parseExpr [] = (undefined, [])
parseExpr (x:xs)
    | elem x ops = parseOpLeft x xs
    | isDigit $ head x = (Number $ read x, xs)
    | otherwise = undefined
    
getExprList :: [String] -> [ExprElem]
getExprList [] = []
getExprList (x:xs)
    | x == ")" || x == "," || x == ";" = []
    | elem x ops = (Op x) : getExprList xs
    | otherwise = (Ast (fst expr)) : getExprList (snd expr)
        where
            expr = parseExpr (x:xs)

infixToPostfix :: [ExprElem] -> [ExprElem]
infixToPostfix list = reverse valueList ++ opList
    where
        (opList, valueList) = infixToPostfixReq list [] []

infixToPostfixReq :: [ExprElem] -> [ExprElem] -> [ExprElem] -> ([ExprElem], [ExprElem])
infixToPostfixReq [] a b = (a, b)
infixToPostfixReq ((Ast x):xs) opList valueList = infixToPostfixReq xs opList ((Ast x):valueList)
infixToPostfixReq ((Op x):xs) opList valueList = infixToPostfixReq xs ((Op x):opList2) valueList2
    where
        (opList2, valueList2) = popOperators (Op x) opList valueList

popOperators :: ExprElem -> [ExprElem] -> [ExprElem] -> ([ExprElem], [ExprElem])
popOperators (Op op) opList valueList =
    if not (null opList) && (getPrecedence op) < (getPrecedence $ getStringFromOp $ head opList)
        then popOperators (Op op) (tail opList) ((head opList) : valueList)
        else (opList, valueList)

getPrecedence :: String -> Integer
getPrecedence op = opPrecedence !! (fromJust $ elemIndex op ops)

getStringFromOp :: ExprElem -> String
getStringFromOp (Op op) = op