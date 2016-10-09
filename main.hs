import Data.Char
import Data.List
import Data.Maybe
import System.Environment

data Ast = Number Integer | Name String | App Op [Ast] | Block [Ast]
    deriving (Show)

data Op = Op
    { symbol :: String
    , numArgs :: Integer
    , precedence :: Integer
    , assoc :: Integer
    }

data ExprElem = Operator Op | Ast Ast
    deriving (Show)

instance Show Op where
    show (Op {symbol=s}) = show s

type AsmLine = String
type Asm = [AsmLine]

type Register = Integer
type Registers = [Bool]

operators = [(Op "+" 2 1 0), (Op "-" 2 1 0), (Op "*" 2 2 0), (Op "/" 2 2 0), (Op "++" 1 3 0)]
extraSymbols = [";", "(", ")"]

opShortList = ["+", "-", "*", "/", "++"]

--- TOKENIZE

tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs)
    | isDigit x = (x : takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
    | isAlpha x = (x : takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
    | x == ' ' || x == '\t' || x == '\n' = tokenize xs
    | length xs >= 2 && symExists (x:take 2 xs) = (x : (take 2 xs)) : (tokenize (drop 2 xs))
    | length xs >= 1 && symExists (x:take 1 xs) = (x : (take 1 xs)) : (tokenize (tail xs))
    | symExists [x] = [x] : tokenize xs
    | otherwise = error $ "Illegal symbol \"" ++ [x] ++ "\""

symExists :: String -> Bool
symExists sym = elem sym (opShortList ++ extraSymbols)

--- PARSE

parse :: String -> Ast
parse str = fst $ parseBlock $ tokenize str

addAst :: Ast -> Ast -> Ast
addAst (Block list) ast = Block (ast:list)

parseBlock :: [String] -> (Ast, [String])
parseBlock [] = (Block [], [])
parseBlock ("}":xs) = (Block [], ("}":xs))
parseBlock (x:xs) = (addAst (fst block) (fst expr), snd block)
    where 
        expr = parseExpr (x:xs)
        block = parseBlock $ snd expr

parseExpr :: [String] -> (Ast, [String])
parseExpr [] = (undefined, [])
parseExpr (x:xs) = (prefixToTree $ infixToPrefix $ fst exprList, drop 1 $ snd exprList)
    where exprList = getExprList (x:xs)

parseSingleExpr :: [String] -> (Ast, [String])
parseSingleExpr ("(":xs) = parseExpr xs
parseSingleExpr (x:xs) = (Number $ read x, xs)

getExprList :: [String] -> ([ExprElem], [String])
getExprList [] = ([], []) -- error "Unexpected end of expression"
getExprList (x:xs)
    | x == ")" || x == "," || x == ";" = ([], (x:xs))
    | elem x opShortList = let exprList = getExprList xs in ((Operator (getOpFromSym x)) : fst exprList, snd exprList)
    | otherwise =  let exprList = getExprList (snd expr) in ((Ast (fst expr)) : fst exprList, snd exprList)
        where
            expr = parseSingleExpr (x:xs)

prefixToTree :: [ExprElem] -> Ast
prefixToTree list = fst $ prefixToTreeReq list

prefixToTreeReq :: [ExprElem] -> (Ast, [ExprElem])
prefixToTreeReq ((Ast x):xs) = (x, xs)
prefixToTreeReq ((Operator op):xs) =
    if numArgs op == 2
        then (App op ((fst arg2):(fst arg1):[]), snd arg2)
        else if numArgs op == 1
            then (App op [fst arg1], snd arg1)
            else error "Illegal number of args"
    where
        arg1 = prefixToTreeReq xs
        arg2 = prefixToTreeReq $ snd arg1

infixToPrefix :: [ExprElem] -> [ExprElem]
infixToPrefix list = reverse opList ++ valueList
    where
        (opList, valueList) = infixToPostfixReq list [] []

infixToPostfixReq :: [ExprElem] -> [ExprElem] -> [ExprElem] -> ([ExprElem], [ExprElem])
infixToPostfixReq [] a b = (a, b)
infixToPostfixReq ((Ast x):xs) opList valueList = infixToPostfixReq xs opList ((Ast x):valueList)
infixToPostfixReq ((op@(Operator {})):xs) opList valueList = infixToPostfixReq xs (op:opList2) valueList2
    where
        (opList2, valueList2) = popOperators op opList valueList

popOperators :: ExprElem -> [ExprElem] -> [ExprElem] -> ([ExprElem], [ExprElem])
popOperators (Operator op) opList valueList =
    if not (null opList) && (precedence op) < (precedence $ getOpFromExprElem $ head opList)
        then popOperators (Operator op) (tail opList) ((head opList) : valueList)
        else (opList, valueList)

getOpFromExprElem :: ExprElem -> Op
getOpFromExprElem (Operator op) = op

getOpFromSym :: String -> Op
getOpFromSym "+" = operators !! 0
getOpFromSym "-" = operators !! 1
getOpFromSym "*" = operators !! 2
getOpFromSym "/" = operators !! 3

--- TO ASM

toAsm :: Ast -> Registers -> (Asm, Register)
toAsm (Block [x]) regs = (fst $ toAsm x regs, 0)
toAsm (Block (x:xs)) regs = (expr ++ block, 0)
    where
        (expr, _) = toAsm x regs
        (block, _) = toAsm (Block xs) regs

toAsm (Number x) regs = (["mov " ++ (getRegName reg) ++ ", " ++ (show x)], reg)
    where
        reg = regAlloc regs

toAsm (App op exprList) regs
    | symbol op == "+" = (expr1 ++ expr2 ++ ["add " ++ getRegName reg1 ++ ", " ++ getRegName reg2], reg1)
    | symbol op == "-" = (expr1 ++ expr2 ++ ["sub " ++ getRegName reg1 ++ ", " ++ getRegName reg2], reg1)
    | symbol op == "*" = (expr1 ++ expr2 ++ ["mul " ++ getRegName reg1 ++ ", " ++ getRegName reg2], reg1)
    | symbol op == "/" = (expr1 ++ expr2 ++ ["div " ++ getRegName reg1 ++ ", " ++ getRegName reg2], reg1)
        where
            (expr1, reg1) = toAsm (exprList !! 1) regs
            (expr2, reg2) = toAsm (exprList !! 0) (takeReg regs reg1)

takeReg :: Registers -> Register -> Registers
takeReg regs reg = take (fromIntegral reg) regs ++ [False] ++ drop (fromIntegral reg + 1) regs

freeReg :: Registers -> Register -> Registers
freeReg regs reg = take (fromIntegral reg) regs ++ [True] ++ drop (fromIntegral reg + 1) regs

regAlloc :: Registers -> Register
regAlloc regs =
    if reg == length regs
        then error "No more regs"
        else toInteger reg
    where
        reg = length $ takeWhile (== False) regs

emptyRegList :: Registers
emptyRegList = [True, True, True, True]

getRegName :: Register -> String
getRegName i = ["rax", "rbx", "rcx", "rdx"] !! fromInteger i

--- COMPILE

compile :: String -> Asm
compile str = fst $ toAsm (parse str) emptyRegList

--- MAIN

main :: IO ()
main = do
    args <- getArgs
    printAsm $ compile $ head args

printAsm [] = do
    putStr ""
printAsm asm = do
    putStrLn $ head asm
    printAsm $ tail asm