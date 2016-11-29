module ParseDecl ( parseDecl ) where

import Data.Char
import Data.Maybe
import Data.List

import Type

parseDecl :: [String] -> (Type, String, [String])
parseDecl (x:xs) =
    let (t, n, i) = parseDecl' (x:xs) 0
        in (t, n, drop i (x:xs))

parseDecl' :: [String] -> Int -> (Type, String, Int)
parseDecl' (x:xs) s =
    if isJust p
        then let (t, i') = parseDeclRight (x:xs) i l r
                 n = if l == r - 2
                    then (x:xs) !! (l + 1)
                    else ""
            in (addType t $ PrimType $ fromJust p, n, i' - 1)
        else error "No type found"
    where
        (p, i) = getPrim (x:xs) s
        (l, r) = findDeclStart (x:xs) i

parseDeclLeft :: [String] -> Int -> Int -> Int -> (Type, Int)
parseDeclLeft (x:xs) start l r
    | l < start = (EmptyType, r)
    | (x:xs) !! l == "*" = let (a, b) = parseDeclLeft (x:xs) start (l - 1) r
                               in (PtrType a, b)
    | (x:xs) !! l == "(" = parseDeclRight (x:xs) start (l - 1) r

parseDeclRight :: [String] -> Int -> Int -> Int -> (Type, Int)
parseDeclRight (x:xs) start l r
    | (x:xs) !! r == "(" =
        let (args, r') = parseDeclArgs (x:xs) (r + 1)
            (ret, end) = parseDeclRight (x:xs) start l r'
            in (FuncType ret args, end)
    | (x:xs) !! r == "[" && (x:xs) !! (r + 1) == "]" =
        let (t, i) = parseDeclRight (x:xs) start l (r + 2)
            in (ArrayType t 0, i)
    | otherwise = parseDeclLeft (x:xs) start l (r + 1)

parseDeclArgs :: [String] -> Int -> ([(Type, String)], Int)
parseDeclArgs (x:xs) i
    | (x:xs) !! i == ")" = ([], i + 1)
    | (x:xs) !! i' == "," = let (ts, i'') = parseDeclArgs (x:xs) i'
                                in ((t, n):ts, i'')
    | otherwise = ([(t, n)], i' + 1)
    where
        (t, n, i') = parseDecl' (x:xs) i

findDeclStart :: [String] -> Int -> (Int, Int)
findDeclStart l s =
    if isJust j
        then if isJust i && fromJust i < fromJust j
            then let i' = fromJust i in (s + i' - 1, s + i' + 1)
            else let j' = fromJust j in (s + j' - 1, s + j')
        else error "No end of statement found"
    where
        i = findIndex isValidVar $ drop s l
        j = findIndex (\ s -> isClosingSym s || s == "[") $ drop s l

addType :: Type -> Type -> Type
addType (PrimType _) _ = error "Cannot add to primitive type"
addType EmptyType b = b
addType (PtrType a) b = (PtrType (addType a b))
addType (FuncType a c) b = (FuncType (addType a b) c)
addType (ArrayType a i) b = (ArrayType (addType a b) i)

getMod :: [String] -> (Maybe String, [String])
getMod (x:xs)
    | x == "static" || x == "extern" = (Just x, xs)
    | otherwise = (Nothing, (x:xs))

prims = [("int", 4), ("short", 2), ("char", 1), ("float", 4), ("double", 8)]

getPrim :: [String] -> Int -> (Maybe String, Int)
getPrim str i = 
    if elem (str !! i) $ fst $ unzip prims
        then (Just (str !! i), i + 1)
        else (Nothing, i)
 
isClosingSym :: String -> Bool
isClosingSym = (flip elem) [";", ",", ")", "]"]

isValidVar :: String -> Bool
isValidVar = all isAlpha