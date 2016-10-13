import Data.Char
import Data.List
import Data.Maybe
import System.Environment

data Ast = Number Integer | Name String | App Op [Ast] | Block [Ast] | Decl Type String | If Ast Ast Ast | Call Ast [Ast]
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

data Type = Type
    { name :: String
    , size :: Integer
    }

instance Show Type where
    show (Type {name=n}) = show n

data AsmFunc = Add Reg Reg | Sub Reg Reg | Mul Reg Reg | Div Reg Reg | Mov Reg Integer | Load Reg String | Save String Reg | SaveToPtr Reg Reg | Label String | Cmp Reg | Jmp String | Je String | Jne String | Jle String | Jl String
    deriving (Show)
type Reg = Integer

type AsmLine = String
type Asm = [AsmFunc]

type Register = Integer
type Registers = [Bool]

data Var = Var
    { varName :: String
    , varType :: Type
    }

operators = [(Op "+" 2 1 0), (Op "-" 2 1 0), (Op "*" 1 2 0), (Op "/" 2 2 0), (Op "++" 1 3 0), (Op "=" 2 0 0)]
extraSymbols = [";", "(", ")", "{", "}", ","]

opShortList = ["+", "-", "*", "/", "++", "="]

types = [(Type "int" 4), (Type "short" 2), (Type "byte" 1)]
typeShortList = ["int", "short", "byte"]

--- TOKENIZE

tokenize :: String -> [String]
tokenize [] = []
tokenize ('/':'/':xs) = tokenize $ dropWhile (/= '\n') xs
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
parseBlock ("}":xs) = (Block [], xs)
parseBlock (x:xs) = (addAst (fst block) (fst expr), snd block)
    where
        expr = parseLine (x:xs)
        block = parseBlock $ snd expr

parseLine :: [String] -> (Ast, [String])
parseLine [] = (undefined, [])
parseLine ("if":"(":xs) = parseIf xs
parseLine (x:xs)
    | isType x && length xs > 0 && all isAlpha (head xs) = (Decl (getTypeFromSym x) (head xs), xs)
    | otherwise = (fst expr, drop 1 $ snd expr)
        where expr = parseExpr (x:xs)

isType :: String -> Bool
isType str = elem str typeShortList

parseIf :: [String] -> (Ast, [String])
parseIf (x:xs) =
    if length (snd block1) > 0 && (snd block1) !! 0 == "else"
        then (If (fst expr) (fst block1) (fst block2), snd block2)
        else (If (fst expr) (fst block1) (Block []), snd block1)
    where
        expr = parseExpr (x:xs)
        block1 = parseExprOrBlock $ drop 1 $ snd expr
        block2 = parseExprOrBlock $ drop 1 $ snd block1

parseCallArgs :: [String] -> ([Ast], [String])
parseCallArgs ("(":")":xs) = ([], xs)
parseCallArgs (")":xs) = ([], xs)
parseCallArgs (x:xs) = (arg : nextArgs, rest)
    where
        (arg, nextArgsString) = parseExpr xs
        (nextArgs, rest) = parseCallArgs nextArgsString

parseExpr :: [String] -> (Ast, [String])
parseExpr [] = (undefined, [])
parseExpr (x:xs) = (prefixToTree $ infixToPrefix $ fst exprList, snd exprList)
    where
        exprList = getExprList (x:xs)

parseExprOrBlock :: [String] -> (Ast, [String])
parseExprOrBlock ("{":xs) = parseBlock xs
parseExprOrBlock (x:xs) = (fst expr, tail $ snd expr)
    where expr = parseExpr (x:xs)

parseSingleExpr :: [String] -> (Ast, [String])
parseSingleExpr (x:xs) =
    let expr = if x == "("
        then parseExpr xs
        else if all isDigit x
            then (Number $ read x, (x:xs))
            else if all isAlpha x
                then (Name x, (x:xs))
                else error $ "Unexpected \"" ++ x ++ "\""
        in
            if (snd expr) !! 1 == "("
                then
                    let args = (parseCallArgs $ tail $ snd expr)
                        in (Call (fst expr) (fst args), snd args)
                else (fst expr, tail $ snd expr)

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
getOpFromSym "++" = operators !! 4
getOpFromSym "=" = operators !! 5

getTypeFromSym :: String -> Type
getTypeFromSym "int" = types !! 0
getTypeFromSym "short" = types !! 1
getTypeFromSym "byte" = types !! 2

--- TO ASM

toAsm :: Ast -> Reg -> (Asm, Reg)
toAsm (Block []) nextReg = ([], nextReg)
toAsm (Block [x]) nextReg = toAsm x nextReg
toAsm (Block (x:xs)) nextReg = (expr ++ block, nextReg)
    where
        (expr, _) = toAsm x nextReg
        (block, _) = toAsm (Block xs) nextReg

toAsm (Number x) nextReg = ([Mov (nextReg + 1) x], nextReg + 1)

toAsm (Name name) nextReg = ([Load (nextReg + 1) name], nextReg + 1)

toAsm (App op exprList) nextReg
    | symbol op == "+" = (expr1 ++ expr2 ++ [Add reg2 reg1], reg2)
    | symbol op == "-" = (expr1 ++ expr2 ++ [Sub reg2 reg1], reg2)
    | symbol op == "*" = (expr1 ++ expr2 ++ [Mul reg2 reg1], reg2)
    | symbol op == "/" = (expr1 ++ expr2 ++ [Div reg2 reg1], reg2)
    | symbol op == "=" = handleAssign (head exprList) (last exprList) nextReg
        where
            (expr1, reg1) = toAsm (exprList !! 1) nextReg
            (expr2, reg2) = toAsm (exprList !! 0) reg1

toAsm (If cond thenBlock elseBlock) nextReg
    | not $ isEmpty elseBlock = (condAsm ++ [Cmp condReg, Jne ("then" ++ show condReg)] ++ elseBlockAsm ++ [Jmp ("endif" ++ show condReg), Label ("then" ++ show condReg)] ++ thenBlockAsm ++ [Label ("endif" ++ show condReg)], 0)
    | otherwise = (condAsm ++ [Cmp condReg, Je ("endif" ++ show condReg)] ++ thenBlockAsm ++ [Label ("endif" ++ show condReg)], 0)
    where
        (condAsm, condReg) = toAsm cond nextReg
        (thenBlockAsm, _) = toAsm thenBlock nextReg
        (elseBlockAsm, _) = toAsm elseBlock nextReg

--toAsm (Call name args) nextReg = 

handleAssign :: Ast -> Ast -> Reg -> (Asm, Reg)
handleAssign (Name name) expr nextReg = (exprAsm ++ [Save name assignReg], assignReg)
    where (exprAsm, assignReg) = toAsm expr nextReg

handleAssign (App op [addrExpr]) expr nextReg = (addrAsm ++ exprAsm ++ [SaveToPtr addrReg exprReg], exprReg)
    where
        (addrAsm, addrReg) = toAsm addrExpr nextReg
        (exprAsm, exprReg) = toAsm expr addrReg

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

isEmpty :: Ast -> Bool
isEmpty (Block list) = null list

getConst :: Ast -> String
getConst = head . words . show

getName :: Ast -> String
getName (Name name) = name

--- COMPILE

compile :: String -> Asm
compile str = fst $ toAsm (parse str) 0

--- MAIN

--main :: IO ()
--main = do
--    args <- getArgs
--    printAsm $ compile $ head args

--printAsm [] = do
    --putStr ""
--printAsm asm = do
    --putStrLn $ head asm
    --printAsm $ tail asm