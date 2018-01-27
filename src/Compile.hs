module Compile ( compile ) where

import Data.Maybe

import Scope
import Type

underscore = "_"

----- ENVIRONMENT -----

type Env = ([String], Reg, Scope)
type Reg = Int
type Id = Int

startReg = 0
emptyEnv = ([], startReg, emptyScope)
regs = ["eax", "ebx", "ecx", "edx"]

envGetAsm :: Env -> [String]
envGetAsm (s, _, _) = s

getRegFromEnv :: Env -> String
getRegFromEnv (_, r, _) = regs !! r

envFreeReg :: Env -> Env
envFreeReg (a, r, s) = (a, r - 1, s)

addLineToEnv :: String -> Env -> Env
addLineToEnv str (xs, r, s) = (str:xs, r, s)

nextReg :: Env -> Env
nextReg (env, r, s) = (env, r + 1, s)

envVarExists :: String -> Env -> Bool
envVarExists str (_, _, scope) = scopeHasVar scope str

envVarType :: String -> Env -> Type
envVarType str (_, _, scope) = PrimType "int"

envGetScope :: Env -> Scope
envGetScope (_, _, s) = s

envSetScope :: Scope -> Env -> Env
envSetScope s (a, r, _) = (a, r, s)

----- BASIC MONAD -----

type Error = String
data Compiler a = C (Env -> Either (Env, a) Error)

instance Monad Compiler where
    return x = C $ \ inp -> Left (inp, x)
--    (>>) (C a) (C b) = C $ \ inp -> either (b . fst) Right $ a inp
    (>>=) (C a) b = C $ \ inp -> case a inp of
        Left (inp', r) -> let (C c) = b r in c inp'
        Right e -> Right e

failure :: Error -> Compiler a
failure e = C $ \ _ -> Right e

failIf :: Bool -> Error -> Compiler ()
failIf True e = failure e
failIf False _ = return ()

compile :: File -> Either [String] Error
compile s = either (Left . envGetAsm . fst) Right $ c emptyEnv
    where (C c) = compileFile s

loop :: (Monad m) => (a -> m ()) -> [a] -> m ()
loop _ [] = return ()
loop f (x:xs) = f x >> loop f xs

addLine :: String -> Compiler ()
addLine str = C $ \ env -> Left (addLineToEnv str env, ())

getReg :: Compiler String
getReg = C $ \ env -> Left (nextReg env, getRegFromEnv env)

freeReg :: Compiler ()
freeReg = C $ \ env -> Left (envFreeReg env, ())

borrowReg :: Compiler String
borrowReg = C $ \ env -> Left (env, getRegFromEnv env)

varExists :: String -> Compiler Bool
varExists str = getScope >>= return . flip scopeHasVar str

getVarType :: String -> Compiler Type
getVarType str = do
    sc <- getScope
    let v = scopeGetVar sc str
    failIf (v == Nothing) $ "Variable does not exist: " ++ str
    return $ varType $ fromJust v

addGlo :: Var -> Compiler ()
addGlo var = getScope >>= setScope . flip scopeAddGlo var

addStc :: Var -> Compiler ()
addStc var = getScope >>= setScope . flip scopeAddStc var

addLoc :: Var -> Compiler ()
addLoc var = getScope >>= setScope . flip scopeAddLoc var

addFun :: Fun -> Compiler ()
addFun fun = getScope >>= setScope . flip scopeAddFun fun

getScope :: Compiler Scope
getScope = C $ \ env -> Left (env, envGetScope env)

setScope :: Scope -> Compiler ()
setScope s = C $ \ env -> Left (envSetScope s env, ())

----- COMPLIE -----

data File = File [Symb]
data Symb = FunDecl Type String | Func Type String [Stmt] | VarDecl Type String Bool | Init Type String Expr
data Stmt = If Expr Stmt Stmt | Nop | Expr Expr | Block [Stmt] | LocVar Type String Bool
data Expr = Number Integer | Name String | App String [Expr] | Call Expr [Expr] | Literal String

compileFile :: File -> Compiler ()
compileSymb :: Symb -> Compiler ()
compileStmt :: Stmt -> Compiler ()
compileExpr :: Expr -> Compiler (String, Type)

compileFile (File s) = loop compileSymb s

compileSymb (VarDecl typ name False) = addGlo $ Var name typ Nothing True
compileSymb (VarDecl typ name True)  = addStc $ Var name typ Nothing True
compileSymb (Init typ name expr) = addGlo $ Var name typ (Just $ evaluate expr) True
compileSymb (FunDecl (FuncType retType args) name) = addFun $ Fun name retType args False 0

compileSymb (Func (FuncType retType args) name body) = do
    addFun (Fun name retType args True $ countLocals body)
    addLine $ name ++ ":"
    sc <- getScope
    loop compileStmt body
    setScope sc

compileStmt (Block body) = do
    sc <- getScope
    loop compileStmt body
    setScope sc

compileStmt (LocVar typ name _) = addLoc $ Var name typ Nothing True

compileStmt (If cond st1 st2) = do
    (reg, _) <- compileExpr cond
    addLine $ "cmp " ++ reg
    addLine "jz else"
    compileStmt st1
    addLine "jmp end"
    addLine "else:"
    compileStmt st2
    addLine "end:"

compileStmt Nop = return ()

compileStmt (Expr expr) = compileExpr expr >> return ()

compileExpr (Number x) = do
    reg <- getReg
    addLine $ "mov " ++ (show x) ++ ", " ++ reg
    return (reg, fromJust $ getIntType x)

compileExpr (Name name) = do
    exists <- varExists name
    failIf (not exists) $ "Missing variable " ++ show name
    reg <- getReg
    addLine $ "load " ++ name ++ " to " ++ reg
    typ <- getVarType name
    return (reg, typ)

compileExpr (App "+" [expr1, expr2]) = do
    (reg1, type1) <- compileExpr expr1
    (reg2, type2) <- compileExpr expr2
    let retType = getType "+" type1 type2
    if not $ isJust retType
        then failure $ "Incompatible types"
        else do
            fixPtrOffset reg1 type1
            fixPtrOffset reg2 type2
            addLine $ "add " ++ reg2 ++ ", " ++ reg1
            return (reg2, fromJust retType)

compileExpr (Call (Name name) args) = do
    retType <- getVarType name
    reg <- getReg
    if reg /= "eax"
        then addLine $ "mov " ++ reg ++ ", eax"
        else return ()
    handleCallArgs args
    addLine $ "call " ++ underscore ++ name
    addLine $ "add esp, " ++ (show $ length args * 4)
    if reg /= "eax"
        then do
            reg2 <- getReg
            addLine $ "mov " ++ reg2 ++ ", eax"
            addLine $ "mov eax, " ++ reg
            return (reg2, retType)
        else return (reg, retType)

handleCallArgs :: [Expr] -> Compiler ()
handleCallArgs = loop $ \ x -> do
    (reg, _) <- compileExpr x
    addLine $ "push " ++ reg
    
fixPtrOffset :: String -> Type -> Compiler ()
fixPtrOffset reg1 typ =
    maybe (return ())
        (\ t -> do
            reg2 <- borrowReg
            addLine $ "mov " ++ reg2 ++ ", " ++ (show $ getTypeSize t)
            addLine $ "mul " ++ reg1 ++ ", " ++ reg2
        )
        $ getPtrType typ

countLocals :: [Stmt] -> Int
countLocals [] = 0
countLocals ((LocVar typ _ False):xs) = getTypeSize typ + countLocals xs
countLocals ((Block e):xs) = countLocals e + countLocals xs
countLocals ((If _ e1 e2):xs) = countLocals [e1] + countLocals [e2] + countLocals xs
countLocals (_:xs) = countLocals xs

evaluate :: Expr -> Value
evaluate (Number x) = Integer $ fromInteger x
evaluate (Literal s) = String s