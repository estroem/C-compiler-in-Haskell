module ParseMonad ( parseMonad ) where

decl :: Parser Type
decl = do
    prim <- primitive
    rest <- (funcDecl <|> pointerDecl <|> arrayDecl)
    return $ addType rest prim
    
decl' = do
    (funcDecl <|> pointerDecl <|> arrayDecl)