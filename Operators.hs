module Operators where

-- Definición de tipo para los términos.
data Term = TrueTerm
          | FalseTerm
          | Var Char
          | Not Term
          | BinOp BinaryOperator Term Term

data BinaryOperator = Or | And | Imply | Equal | Unequal

-- Tipo para las ecuaciones.
data Equation = Equiv Term Term

-- Constantes y operadores.
true = TrueTerm 
false = FalseTerm
neg = Not

infixl 4 \/, /\  
(\/) = BinOp Or
(/\) = BinOp And

infixr 3 ==>
(==>) = BinOp Imply

infixl 2 <==>, !<==>
(<==>) = BinOp Equal
(!<==>) = BinOp Unequal

infixl 1 ===
(===) = Equiv

-- Para imprimir las expresiones.
instance Show Term where
    show TrueTerm = "true"
    show FalseTerm = "false"
    show (Var x) = [x] 
    show (Not (Var x)) = "neg " ++ show (Var x)
    show (Not t) = "neg (" ++ show t ++ ")"
    show (BinOp op (Var x) (Var y)) = show (Var x) ++ show op ++ show (Var y)
    show (BinOp op (Var x) t) = show (Var x) ++ show op ++ "(" ++ show t ++ ")"
    show (BinOp op t (Var x)) = "(" ++ show t ++ ")" ++ show op ++ show (Var x)
    show (BinOp op t1 t2) = "(" ++ show t1 ++ ")" ++ show op ++ "(" ++ show t2 ++ ")"

instance Show BinaryOperator where
    show Or = " \\/ "
    show And = " /\\ "
    show Imply = " ==> "
    show Equal = " <==> "
    show Unequal = " !<==> "

-- Para poder ver el resultado de las instanciaciones:
instance Show Equation where
    show (Equiv t1 t2) = show t1 ++ " === " ++ show t2
