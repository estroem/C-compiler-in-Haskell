import Data.Char
import Data.List
import Data.Maybe
import System.Environment

data Ast = Number Integer | Name String | App Op [Ast] | Block [Ast] | Decl Type String
         | If Ast Ast Ast | Call Ast [Ast] | DeclInit Type String String
         | Func Type String [(Type, String)] Ast | File [Ast]
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

--data Type = PrimType { primTypeName :: String } | PtrType { ptrTypePointsTo :: Type }
--          | FuncType { funcTypeRet :: Type, funcTypeArgs :: [Type] }

--instance Show Type where
--    show (Type {name=n}) = show n

data Type = PrimType String | PtrType Type | FuncType Type [Type] | ArrayType Type | EmptyType
    deriving (Show)

data RtlLine = Add Reg Reg | Sub Reg Reg | Mul Reg Reg | Div Reg Reg | Mov Reg Integer
             | Load Reg String | Save String Reg | SaveToPtr Reg Reg | Label String
             | Cmp Reg | Jmp String | Je String | Jne String | Jle String | Jl String
             | CallName String [Reg] Reg | CallAddr Reg [Reg] Reg | DeRef Reg
    deriving (Show)
type Reg = Integer

type Rtl = [RtlLine]

type Register = Integer
type Registers = [Bool]

data Var = Var
    { varName :: String
    , varType :: Type
    }

operators = [(Op "+" 2 1 0), (Op "-" 2 1 0), (Op "*" 1 2 0), (Op "/" 2 2 0), (Op "++" 1 3 0), (Op "=" 2 0 0), (Op "$" 1 4 0)]
extraSymbols = [";", "(", ")", "{", "}", ","]

opShoRtlist = ["+", "-", "*", "/", "++", "=", "$"]

--types = [(Type "int" 4), (Type "short" 2), (Type "byte" 1)]
typeShoRtlist = ["int", "short", "byte"]

--- TOKENIZE

tokenize :: String -> [String]
tokenize [] = []
tokenize ('/':'/':xs) = tokenize $ dropWhile (/= '\n') xs
tokenize (x:xs)
    | isDigit x = (x : takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
    | isAlpha x = (x : takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
    | x == '"' = (x : takeWhile (/= '"') xs) : tokenize (tail $ dropWhile (/= '"') xs)
    | x == ' ' || x == '\t' || x == '\n' = tokenize xs
    | length xs >= 2 && symExists (x:take 2 xs) = (x : (take 2 xs)) : (tokenize (drop 2 xs))
    | length xs >= 1 && symExists (x:take 1 xs) = (x : (take 1 xs)) : (tokenize (tail xs))
    | symExists [x] = [x] : tokenize xs
    | otherwise = error $ "Illegal symbol \"" ++ [x] ++ "\""

symExists :: String -> Bool
symExists sym = elem sym (opShoRtlist ++ extraSymbols)

--- PARSE

parse :: String -> Ast
parse str = fst $ parseBlock $ tokenize str

addAst :: Ast -> Ast -> Ast
addAst (Block list) ast = Block (ast:list)
addAst (File list) ast = File (ast:list)

parseFile :: [String] -> (Ast, [String])
parseFile [] = (File [], [])
parseFile (x:xs) = (addAst file line, final)
    where
        (line, rest) = parseTopLvlLine (x:xs)
        (file, final) = parseFile rest

parseTopLvlLine :: [String] -> (Ast, [String])
parseTopLvlLine (x:xs) =
    case decl of
        (PrimType prim)      -> if (head rest) == "=" && (rest !! 2) == ";" then (DeclInit decl name (rest !! 1), drop 3 rest) else if (head rest) == ";" then (Decl decl name, tail rest) else error "Expected ; or ="
        (PtrType ptr)        -> if (head rest) == "=" && (rest !! 2) == ";" then (DeclInit decl name (rest !! 1), drop 3 rest) else if (head rest) == ";" then (Decl decl name, tail rest) else error "Expected ; or ="
        (FuncType func args) -> let block = parseExprOrBlock rest in (Func decl name [] (fst block), snd block)
    where
        (decl, name, rest) = parseDecl (x:xs)

parseDecl :: [String] -> (Type, String, [String])
parseDecl (x:xs) = (addType a (PrimType x), b, c)
    where (a, b, c) = parseDeclReq xs

parseDeclReq :: [String] -> (Type, String, [String])
parseDeclReq ("(":xs) = 
    if (head afterDef) == ")"
        then if (afterDef !! 1) == "("
            then (addType def (FuncType EmptyType args), name, afterArgs)
            else (def, name, tail afterDef)
        else error $ "Unexpected " ++ (head afterDef) ++ ", exptected )"
    where
        (def, name, afterDef) = parseDeclReq xs
        (args, afterArgs) = parseFuncArgs $ drop 2 afterDef

parseDeclReq (")":xs) = (EmptyType, "", ")":xs)
parseDeclReq ("*":xs) = (addType a (PtrType EmptyType), b, c)
    where
        (a, b, c) = parseDeclReq xs

parseDeclReq (x:xs) =
    if (head xs) == "("
        then ((FuncType EmptyType args), x, rest)
        else (EmptyType, x, xs)
    where
        (args, rest) = parseFuncArgs $ tail xs

addType :: Type -> Type -> Type
addType (PrimType a) b = error ""
addType EmptyType b = b
addType (PtrType a) b = (PtrType (addType a b))
addType (FuncType a c) b = (FuncType (addType a b) c)

parseFuncArgs :: [String] -> ([Type], [String])
parseFuncArgs (")":xs) = ([], xs)
parseFuncArgs (x:xs) = (a : argList, rest)
    where
        (a, b, c) = parseDecl (x:xs)
        (argList, rest) = parseFuncArgs c
    
parseBlock :: [String] -> (Ast, [String])
parseBlock [] = (Block [], [])
parseBlock (";":xs) = (Block [], xs)
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
isType str = elem str typeShoRtlist

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
parseExprOrBlock (";":xs) = (Block [], xs)
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
    | elem x opShoRtlist = let exprList = getExprList xs in ((Operator (getOpFromSym x)) : fst exprList, snd exprList)
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
getOpFromSym sym = fromJust $ find (\ op -> symbol op == sym) operators

getTypeFromSym :: String -> Type
getTypeFromSym sym = (PrimType sym) --fromJust $ find (\ n -> name n == sym) types

--- TO Rtl

treeToRtl :: Ast -> Rtl
treeToRtl tree = fst $ toRtl tree 0

toRtl :: Ast -> Reg -> (Rtl, Reg)
toRtl (Block []) nextReg = ([], nextReg)
toRtl (Block [x]) nextReg = toRtl x nextReg
toRtl (Block (x:xs)) nextReg = (expr ++ block, nextReg)
    where
        (expr, _) = toRtl x nextReg
        (block, _) = toRtl (Block xs) nextReg

toRtl (Number x) nextReg = ([Mov (nextReg + 1) x], nextReg + 1)

toRtl (Name name) nextReg = ([Load (nextReg + 1) name], nextReg + 1)

toRtl (App op exprList) nextReg
    | symbol op == "+" = (expr1 ++ expr2 ++ [Add reg2 reg1], reg2)
    | symbol op == "-" = (expr1 ++ expr2 ++ [Sub reg2 reg1], reg2)
    | symbol op == "*" = (expr1 ++ expr2 ++ [Mul reg2 reg1], reg2)
    | symbol op == "/" = (expr1 ++ expr2 ++ [Div reg2 reg1], reg2)
    | symbol op == "=" = handleAssign (head exprList) (last exprList) nextReg
    | symbol op == "$" = (expr ++ [DeRef reg], reg)
        where
            (expr1, reg1) = toRtl (exprList !! 1) nextReg
            (expr2, reg2) = toRtl (exprList !! 0) reg1
            (expr, reg) = toRtl (head exprList) nextReg

toRtl (If cond thenBlock elseBlock) nextReg
    | not $ isEmpty elseBlock = (condRtl ++ [Cmp condReg, Jne ("then" ++ show condReg)] ++ elseBlockRtl ++ [Jmp ("endif" ++ show condReg), Label ("then" ++ show condReg)] ++ thenBlockRtl ++ [Label ("endif" ++ show condReg)], 0)
    | otherwise = (condRtl ++ [Cmp condReg, Je ("endif" ++ show condReg)] ++ thenBlockRtl ++ [Label ("endif" ++ show condReg)], 0)
    where
        (condRtl, condReg) = toRtl cond nextReg
        (thenBlockRtl, _) = toRtl thenBlock nextReg
        (elseBlockRtl, _) = toRtl elseBlock nextReg

toRtl (Call (Name name) args) nextReg = (argsRtl ++ [CallName name argRegs nextReg], nextReg)
    where (argsRtl, argRegs) = handleCallArgs args nextReg

toRtl (Call addr args) nextReg = (addrRtl ++ argsRtl ++ [CallAddr addrReg argRegs nextReg], nextReg)
    where
        (addrRtl, addrReg) = toRtl addr nextReg
        (argsRtl, argRegs) = handleCallArgs args addrReg

handleCallArgs :: [Ast] -> Reg -> (Rtl, [Reg])
handleCallArgs [] _ = ([], [])
handleCallArgs (x:xs) nextReg = (argRtl ++ finalRtl, argReg : finalReg)
    where
        (argRtl, argReg) = toRtl x nextReg
        (finalRtl, finalReg) = handleCallArgs xs argReg

handleAssign :: Ast -> Ast -> Reg -> (Rtl, Reg)
handleAssign (Name name) expr nextReg = (exprRtl ++ [Save name assignReg], assignReg)
    where (exprRtl, assignReg) = toRtl expr nextReg

handleAssign (App op [addrExpr]) expr nextReg = (addrRtl ++ exprRtl ++ [SaveToPtr addrReg exprReg], exprReg)
    where
        (addrRtl, addrReg) = toRtl addrExpr nextReg
        (exprRtl, exprReg) = toRtl expr addrReg

isEmpty :: Ast -> Bool
isEmpty (Block list) = null list

getTypeConst :: Type -> String
getTypeConst = head . words . show

getName :: Ast -> String
getName (Name name) = name

--- COMPILE

compile :: String -> Rtl
compile = treeToRtl . parse

--- MAIN

--main :: IO ()
--main = do
--    args <- getArgs
--    printRtl $ compile $ head args

--printRtl [] = do
    --putStr ""
--printRtl Rtl = do
    --putStrLn $ head Rtl
    --printRtl $ tail Rtl