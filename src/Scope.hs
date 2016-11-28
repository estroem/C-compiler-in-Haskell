module Scope ( Scope (..), Var (..), Fun (..), Value (..),
               emptyScope, scopeAddGlo, scopeAddStc, scopeAddPar,
               scopeAddLoc, scopeAddFun, getOffset, scopeHasVar,
               scopeHasFun, scopeGetVar, scopeGetFun,
               joinScopes, hideLocals ) where

import Data.List
import Data.Maybe

import Type

data Var = Var
    { varName :: String
    , varType :: Type
    , varValue :: Maybe Value
    , varIsVis :: Bool
    } deriving (Show)

data Fun = Fun
    { funName :: String
    , funRetType :: Type
    , funArgs :: [(Type, String)]
    , funIsDef :: Bool
    , numLocals :: Integer
    } deriving (Show)

data Value = Integer Integer | Float Float | String String
    deriving (Show)

--           Scope globals statics locals functions
data Scope = Scope [Var] [Var] [Var] [Var] [Fun]
    deriving (Show)

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
scopeHasVar (Scope gs ss ps ls fs) name = any (\ v -> (varName v) == name && varIsVis v) (gs ++ ss ++ ps ++ ls)

scopeHasFun :: Scope -> String -> Bool
scopeHasFun (Scope gs ss ps ls fs) name = any (\ f -> (funName f) == name) fs

scopeGetVar :: Scope -> String -> Maybe Var
scopeGetVar (Scope gs ss ps ls fs) name = find (\ v -> (varName v) == name) (gs ++ ss ++ ps ++ ls)

scopeGetFun :: Scope -> String -> Maybe Fun
scopeGetFun (Scope gs ss ps ls fs) name = find (\ f -> (funName f) == name) fs

joinScopes :: [Scope] -> Scope
joinScopes list = joinScopesLoop list emptyScope where
    joinScopesLoop ((Scope gs ss ps ls fs):xs) (Scope rgs rss rps rls rfs) =
        joinScopesLoop xs (Scope (rgs ++ gs) (rss ++ ss) (rps ++ ps) (rls ++ ls) (rfs ++ fs))
    joinScopesLoop [] res = res

hideLocals :: Scope -> Scope
hideLocals (Scope gs ss ps ls fs) = Scope gs ss ps (map (\ (Var n t v _) -> Var n t v False) ls) fs