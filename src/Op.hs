module Op ( Op (..), getOpFromSym, isOperator ) where

import Data.Maybe
import Data.List

data Op = Op
    { symbol :: String
    , numArgs :: Integer
    , precedence :: Integer
    , assoc :: Integer
    }

instance Show Op where
    show (Op {symbol=s}) = show s

operators =
    [(Op "+" 2 9 0),
     (Op "-" 2 9 0),
     (Op "*" 1 10 0),
     (Op "/" 2 10 0),
     (Op "++" 1 11 0),
     (Op "=" 2 0 0),
     (Op "$" 1 11 0),
     (Op "==" 2 6 0),
     (Op "!=" 2 6 0),
     (Op "<" 2 7 0),
     (Op ">" 2 7 0),
     (Op "<=" 2 7 0),
     (Op ">=" 2 7 0),
     (Op "!" 1 11 0),
     (Op "&" 1 11 0)]

getOpFromSym :: String -> Op
getOpFromSym sym = fromJust $ find (\ op -> symbol op == sym) operators

isOperator :: String -> Bool
isOperator sym = any (\ op -> symbol op == sym) operators