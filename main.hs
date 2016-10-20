import Data.Char
import Data.List
import Data.Maybe
import System.Environment

data Ast = Number Integer | Name String | App Op [Ast] | Block [Ast] | VarDecl Type String Bool Bool
         | If Ast Ast Ast | Call Ast [Ast] | Init Type String Ast
         | Func Type String Ast | File [Ast] | FunDecl Type String
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

data Type = PrimType String | PtrType Type | FuncType Type [(Type, String)] | ArrayType Type | EmptyType
    deriving (Show)

data RtlLine = Add Reg Reg | Sub Reg Reg | Mul Reg Reg | Div Reg Reg | Mov Reg Integer
             | Load Reg String | Save String Reg Integer | SaveToPtr Reg Reg Integer | Label String
             | Cmp Reg | Jmp String | Je String | Jne String | Jle String | Jl String
             | CallName String [Reg] Reg | CallAddr Reg [Reg] Reg | DeRef Reg
             | FuncStart String | FuncEnd String | Return | Push Reg | LoadLoc Reg Integer
             | SaveLoc Integer Reg | Pop Reg | AddConst Reg Integer | SubConst Reg Integer
             | MulConst Reg Integer | DivConst Reg Integer
    deriving (Show)
type Reg = Integer

type Rtl = [RtlLine]

type Register = Integer
type Registers = [Bool]

data Var = Var
    { varName :: String
    , varType :: Type
    } deriving (Show)

data Fun = Fun
    { funName :: String
    , funRetType :: Type
    , funArgs :: [(Type, String)]
    , funIsDef :: Bool
    } deriving (Show)

reg_ebp :: Integer
reg_ebp = -2
reg_esp :: Integer
reg_esp = -1

--           Scope globals statics locals functions
data Scope = Scope [Var] [Var] [Var] [Var] [Fun]
    deriving (Show)

type AsmLine = String
type Asm = [AsmLine]

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
parse str = fst $ parseFile $ tokenize str

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
        (FuncType _ _) -> if (head rest) == ";"
                              then (FunDecl decl name, tail rest)
                              else let block = parseExprOrBlock rest
                                  in (Func decl name (fst block), snd block)
        _              -> if (head rest) == "="
                            then (Init decl name expr, tail exprRest)
                            else if (head rest) == ";"
                                then (VarDecl decl name True False, tail rest)
                                else error "Expected ; or ="
    where
        (decl, name, rest) = parseDecl (x:xs)
        (expr, exprRest) = parseExpr $ drop 1 rest

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
        else if isAlpha $ head x then (EmptyType, x, xs) else error $ "Illegal variable \"" ++ x ++ "\". Variables must start with letter"
    where
        (args, rest) = parseFuncArgs $ tail xs

addType :: Type -> Type -> Type
addType (PrimType a) b = error ""
addType EmptyType b = b
addType (PtrType a) b = (PtrType (addType a b))
addType (FuncType a c) b = (FuncType (addType a b) c)

parseFuncArgs :: [String] -> ([(Type, String)], [String])
parseFuncArgs (")":xs) = ([], xs)
parseFuncArgs (",":xs) = parseFuncArgs xs
parseFuncArgs (x:xs) = ((a, b) : argList, rest)
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
    | isType x && length xs > 0 && all isAlpha (head xs) = (VarDecl (getTypeFromSym x) (head xs) False False, xs)
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

emptyScope :: Scope
emptyScope = (Scope [] [] [] [] [])

scopeAddGlo :: Scope -> Var -> Scope
scopeAddGlo (Scope gs ss ps ls fs) v = (Scope (v:gs) ss ps ls fs)

scopeAddStc :: Scope -> Var -> Scope
scopeAddStc (Scope gs ss ps ls fs) v = (Scope gs (v:ss) ps ls fs)

scopeAddPar :: Scope -> Var -> Scope
scopeAddPar (Scope gs ss ps ls fs) v = (Scope gs ss (v:ps) ls fs)

scopeAddLoc :: Scope -> Var -> Scope
scopeAddLoc (Scope gs ss ps ls fs) v = (Scope gs ss ps (v:ls) fs)

scopeAddFun :: Scope -> Fun -> Scope
scopeAddFun (Scope gs ss ps ls fs) f = (Scope gs ss ps ls (f:fs))

getOffset :: Scope -> String -> Maybe Integer
getOffset (Scope _ _ ps ls _) n =
    let i = findIndex (\ v -> (varName v) == n) ls in
        if isJust i
            then Just (toInteger $ (fromJust i + 1) * (-4))
            else let j = findIndex (\ v -> (varName v) == n) ps in
                if isJust j
                    then Just (toInteger $ (fromJust j + 2) * 4)
                    else Nothing

scopeHasVar :: Scope -> String -> Bool
scopeHasVar (Scope gs ss ps ls fs) name = any (\ v -> (varName v) == name) (gs ++ ss ++ ps ++ ls)

scopeHasFun :: Scope -> String -> Bool
scopeHasFun (Scope gs ss ps ls fs) name = any (\ f -> (funName f) == name) fs

joinScopes :: [Scope] -> Scope
joinScopes list = joinScopesLoop list emptyScope where
    joinScopesLoop ((Scope gs ss ps ls fs):xs) (Scope rgs rss rps rls rfs) =
        joinScopesLoop xs (Scope (gs ++ rgs) (ss ++ rss) (ps ++ rps) (ls ++ rls) (fs ++ rfs))
    joinScopesLoop [] res = res

treeToRtl :: Ast -> (Rtl, Scope)
treeToRtl tree = let (a, b, c) = toRtl tree 0 emptyScope in (a, c)

toRtl :: Ast -> Reg -> Scope -> (Rtl, Reg, Scope)
toRtl (File []) nextReg scope = ([], nextReg, emptyScope)
toRtl (File [x]) nextReg scope = toRtl x nextReg scope
toRtl (File (x:xs)) nextReg scope = (expr ++ file, nextReg, joinScopes [scope1, scope2])
    where
        (expr, _, scope1) = toRtl x nextReg scope
        (file, _, scope2) = toRtl (File xs) nextReg (joinScopes [scope, scope1])

toRtl (Block []) nextReg scope = ([], nextReg, emptyScope)
toRtl (Block [x]) nextReg scope = toRtl x nextReg scope
toRtl (Block (x:xs)) nextReg scope = (expr ++ block, nextReg, joinScopes [scope1, scope2])
    where
        (expr, _, scope1) = toRtl x nextReg scope
        (block, _, scope2) = toRtl (Block xs) nextReg (joinScopes [scope, scope1])

toRtl (Number x) nextReg scope = ([Mov (nextReg + 1) x], nextReg + 1, emptyScope)

toRtl (Name name) nextReg scope =
    if scopeHasVar scope name
        then let i = getOffset scope name in
            if isJust i
                then ([LoadLoc (nextReg + 1) (fromJust i)], nextReg + 1, emptyScope)
                else ([Load (nextReg + 1) name], nextReg + 1, emptyScope)
        else error $ "Variable not in scope: " ++ name

toRtl (VarDecl t n g s) nextReg scope = ([], nextReg, (if g then scopeAddGlo else if s then scopeAddStc else scopeAddLoc) emptyScope (Var n t))

toRtl (App op exprList) nextReg scope
    | symbol op == "+" = (expr1 ++ expr2 ++ [Add reg2 reg1], reg2, emptyScope)
    | symbol op == "-" = (expr1 ++ expr2 ++ [Sub reg2 reg1], reg2, emptyScope)
    | symbol op == "*" = (expr1 ++ expr2 ++ [Mul reg2 reg1], reg2, emptyScope)
    | symbol op == "/" = (expr1 ++ expr2 ++ [Div reg2 reg1], reg2, emptyScope)
    | symbol op == "=" = let (a, b) = handleAssign (head exprList) (last exprList) nextReg scope in (a, b, emptyScope)
    | symbol op == "$" = (expr ++ [DeRef reg], reg, emptyScope)
        where
            (expr1, reg1, _) = toRtl (exprList !! 1) nextReg scope
            (expr2, reg2, _) = toRtl (exprList !! 0) reg1 scope
            (expr, reg, _) = toRtl (head exprList) nextReg scope

toRtl (If cond thenBlock elseBlock) nextReg scope
    | not $ isEmpty elseBlock = (condRtl ++
                                 [Cmp condReg, Jne ("then" ++ show condReg)] ++
                                 elseBlockRtl ++
                                 [Jmp ("endif" ++ show condReg),
                                 Label ("then" ++ show condReg)] ++
                                 thenBlockRtl ++
                                 [Label ("endif" ++ show condReg)],
                                 0, joinScopes [thenNewVars, elseNewVars])
    | otherwise = (condRtl ++ [Cmp condReg, Je ("endif" ++ show condReg)] ++ thenBlockRtl ++ [Label ("endif" ++ show condReg)], 0, thenNewVars)
    where
        (condRtl, condReg, _) = toRtl cond nextReg scope
        (thenBlockRtl, _, thenNewVars) = toRtl thenBlock nextReg scope
        (elseBlockRtl, _, elseNewVars) = toRtl elseBlock nextReg scope

toRtl (Call (Name name) args) nextReg scope = (argsRtl ++
                                               handleArgPush argRegs ++
                                               [CallName ('_':name) argRegs nextReg] ++
                                               [AddConst reg_esp (toInteger $ length args * 4)],
                                               nextReg, emptyScope)
    where (argsRtl, argRegs) = handleCallArgs args nextReg scope

toRtl (Call addr args) nextReg scope = (addrRtl ++
                                        argsRtl ++
                                        handleArgPush argRegs ++
                                        [CallAddr addrReg argRegs nextReg] ++
                                        [AddConst reg_esp (toInteger $ length args * 4)],
                                        nextReg, emptyScope)
    where
        (addrRtl, addrReg, _) = toRtl addr nextReg scope
        (argsRtl, argRegs) = handleCallArgs args addrReg scope

toRtl (Func (FuncType retType argTypes) name body) nextReg scope = ([Label ('_':name), Push reg_ebp, Mov reg_ebp reg_esp] ++
                                                                    bodyRtl ++
                                                                    (if (length ls) > 0
                                                                        then [AddConst reg_esp (toInteger (length ls) * 4)]
                                                                        else []) ++
                                                                    [Pop reg_ebp, Return],
                                                                    nextReg, (Scope [] ss [] [] [Fun name retType argTypes True]))
    where
        (bodyRtl, _, (Scope _ ss _ ls _)) = toRtl body nextReg (joinScopes [(Scope [] [] (argTypesToVars argTypes) [] []), scope])

toRtl (FunDecl (FuncType retType argTypes) name) nextReg scope = ([], nextReg, scopeAddFun emptyScope (Fun name retType argTypes False))

argTypesToVars :: [(Type, String)] -> [Var]
argTypesToVars list = argTypesToVarsLoop list [] where
    argTypesToVarsLoop ((t,n):xs) res = argTypesToVarsLoop xs (res ++ [Var n t])
    argTypesToVarsLoop [] res = res

handleArgPush :: [Reg] -> Rtl
handleArgPush regs = handleArgPushLoop regs [] where
    handleArgPushLoop (x:xs) rtl = handleArgPushLoop xs ((Push x):rtl)
    handleArgPushLoop [] rtl = rtl

handleCallArgs :: [Ast] -> Reg -> Scope -> (Rtl, [Reg])
handleCallArgs [] _ _ = ([], [])
handleCallArgs (x:xs) nextReg scope = (argRtl ++ finalRtl, argReg : finalReg)
    where
        (argRtl, argReg, _) = toRtl x nextReg scope
        (finalRtl, finalReg) = handleCallArgs xs argReg scope

handleAssign :: Ast -> Ast -> Reg -> Scope -> (Rtl, Reg)
handleAssign (Name name) expr nextReg scope =
    if scopeHasVar scope name
        then let i = getOffset scope name in
            if isJust i
                then (exprRtl ++ [SaveLoc (fromJust i) assignReg], assignReg)
                else (exprRtl ++ [Save name assignReg 4], assignReg)
        else error $ "Variable not in scope: " ++ name
    where (exprRtl, assignReg, _) = toRtl expr nextReg scope

handleAssign (App op [addrExpr]) expr nextReg scope = (addrRtl ++ exprRtl ++ [SaveToPtr addrReg exprReg 4], exprReg)
    where
        (addrRtl, addrReg, _) = toRtl addrExpr nextReg scope
        (exprRtl, exprReg, _) = toRtl expr addrReg scope

isEmpty :: Ast -> Bool
isEmpty (Block list) = null list

-- TO ASM

toAsm :: Rtl -> Scope -> Asm
toAsm r (Scope gs ss _ _ fs) = toAsmGlobals fs ++
                               ["section .data"] ++ (map toAsmDataLine $ gs ++ ss) ++
                               ["section .text"] ++ (map toAsmLine r)

toAsmGlobals :: [Fun] -> Asm
toAsmGlobals funs = map (\ f -> "global _" ++ funName f) $ filter funIsDef funs

toAsmDataLine :: Var -> AsmLine
toAsmDataLine (Var n t) = n ++ " " ++ (getSizeWordData $ getSizeInt t) ++ " 0"

toAsmLine :: RtlLine -> AsmLine
toAsmLine (Add reg1 reg2)            = "add " ++ getReg reg1 ++ ", " ++ getReg reg2
toAsmLine (Sub reg1 reg2)            = "sub " ++ getReg reg1 ++ ", " ++ getReg reg2
toAsmLine (Mul reg1 reg2)            = "mul " ++ getReg reg1 ++ ", " ++ getReg reg2
toAsmLine (Div reg1 reg2)            = "div " ++ getReg reg1 ++ ", " ++ getReg reg2
toAsmLine (Mov reg i)                = "mov " ++ getReg reg  ++ ", " ++ show i
toAsmLine (Load reg name)            = "mov " ++ getReg reg  ++ ", [" ++ name ++ "]"
toAsmLine (Save name reg size)       = "mov " ++ (getSizeWord size) ++ " ptr [" ++ name ++ "], " ++ getReg reg
toAsmLine (SaveToPtr reg1 reg2 size) = "mov " ++ (getSizeWord size) ++ " ptr [" ++ getReg reg1 ++ "], " ++ getReg reg2
toAsmLine (Label name)               = name ++ ":"
toAsmLine (Cmp reg)                  = "cmp " ++ getReg reg ++ ", 0"
toAsmLine (Jmp label)                = "jmp " ++ label
toAsmLine (Je label)                 = "je " ++ label
toAsmLine (Jne label)                = "jne " ++ label
toAsmLine (Jle label)                = "jle " ++ label
toAsmLine (Jl label)                 = "jl " ++ label
toAsmLine (CallName name args _)     = "call " ++ name
toAsmLine (CallAddr addr args _)     = "call " ++ getReg addr
toAsmLine (DeRef reg)                = "mov " ++ getReg reg ++ ", [" ++ getReg reg ++ "]"
toAsmLine (Return)                   = "ret"
toAsmLine (Push reg)                 = "push " ++ getReg reg
toAsmLine (Pop reg)                  = "pop " ++ getReg reg
toAsmLine (LoadLoc reg offset)       = "mov " ++ getReg reg ++ ", [esp" ++ (if offset > 0 then "+" else "") ++ show offset ++ "]"
toAsmLine (SaveLoc offset reg)       = "mov [esp" ++ (if offset > 0 then "+" else "") ++ show offset ++ "], " ++ getReg reg
toAsmLine (AddConst reg int)         = "add " ++ getReg reg ++ ", " ++ show int
toAsmLine (SubConst reg int)         = "sub " ++ getReg reg ++ ", " ++ show int
toAsmLine (MulConst reg int)         = "mul " ++ getReg reg ++ ", " ++ show int
toAsmLine (DivConst reg int)         = "div " ++ getReg reg ++ ", " ++ show int

getReg :: Reg -> String
getReg (-2) = "ebp"
getReg (-1) = "esp"
getReg 1 = "eax"
getReg 2 = "ebx"
getReg 3 = "ecx"
getReg 4 = "edx"

getSizeInt :: Type -> Integer
getSizeInt (PtrType _) = 4
getSizeInt (PrimType "int") = 4
getSizeInt (PrimType "short") = 2
getSizeInt (PrimType "byte") = 1

getSizeWordData :: Integer -> String
getSizeWordData 1 = "db"
getSizeWordData 2 = "dw"
getSizeWordData 4 = "dd"

getSizeWord :: Integer -> String
getSizeWord 1 = "byte"
getSizeWord 2 = "word"
getSizeWord 4 = "dword"

--- COMPILE

compile :: String -> Asm
compile = (uncurry toAsm) . treeToRtl . parse

--- MAIN

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ intercalate "\n" $ compile $ head args