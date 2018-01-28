module Main where

----- ENVIRONMENT -----

type Env = ([String], Reg, Scope)
type Reg = Int
type Scope = [String]
type Id = Int

startReg = 0
emptyEnv = ([], startReg, ["f"])
regs = ["eax", "ebx", "ecx", "edx"]

envGetAsm :: Env -> [String]
envGetAsm (s, _, _) = s

getRegFromEnv :: Env -> String
getRegFromEnv (_, r, _) = regs !! r

addLineToEnv :: String -> Env -> Env
addLineToEnv str (xs, r, s) = (str:xs, r, s)

nextReg :: Env -> Env
nextReg (env, r, s) = (env, r + 1, s)

envVarExists :: String -> Env -> Bool
envVarExists str (_, _, scope) = elem str scope

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

varExists :: String -> Compiler Bool
varExists str = C $ \ env -> Left (env, envVarExists str env)

----- COMPLIE -----

data File = File [Symb]
data Symb = Func String [Stmt] | Glob String
data Stmt = If Expr Stmt Stmt | Nop | Expr Expr
data Expr = Number Int | Var String

compileFile :: File -> Compiler ()
compileSymb :: Symb -> Compiler ()
compileStmt :: Stmt -> Compiler ()
compileExpr :: Expr -> Compiler String

compileFile (File s) = loop compileSymb s

compileSymb (Func name body) = do
    addLine $ name ++ ":"
    loop compileStmt body

compileStmt (If cond st1 st2) = do
    reg <- compileExpr cond
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
    return reg
    
compileExpr (Var name) = do
    exists <- varExists name
    if exists
        then do
            reg <- getReg
            addLine $ "load " ++ name ++ " to " ++ reg
            return reg
        else failure $ "Missing variable " ++ show name
----- MAIN -----

tree :: File
tree = File [Func "main" [Expr (Var "s"), If (Number 5) Nop Nop]]
    
main :: IO ()
main = do
    putStrLn $ either show ("Error: "++) $ compile tree