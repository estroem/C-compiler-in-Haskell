module Reg ( Reg, Regs, reg_esp, reg_ebp ) where

type Reg = Int
type Regs = [Reg]

reg_ebp :: Int
reg_esp = -1
reg_esp :: Int
reg_ebp = -2

allRegs :: Regs
allRegs = [0, 1, 2, 3]

getReg :: Regs -> Reg
getReg [] = error "No regs available"
getReg (x:xs) = x

takeReg :: Reg -> Regs -> Regs
takeReg x xs = a ++ tail b
    where (a, b) = span (/=x) xs

freeReg :: Reg -> Regs -> Regs
freeReg x xs
    | elem x xs = error "Reg already free"
    | otherwise = x:xs

regIsTaken :: Reg -> Regs -> Bool
regIsTaken = elem