-- Definición de tipo para los términos.
data Term = TrueTerm
          | FalseTerm
          | Var Char
          | Not Term
          | Or Term Term
          | And Term Term
          | Imply Term Term
          | Equal Term Term
          | Unequal Term Term
          deriving(Show)

-- Tipo para las ecuaciones.
data Equation = Equiv Term Term

-- Constantes y operadores.
true = TrueTerm 
false = FalseTerm
neg = Not

infixl 4 \/, /\  
(\/) = Or
(/\) = And

infixr 3 ==>
(==>) = Imply

infixl 2 <==>, !<==>
(<==>) = Equal
(!<==>) = Unequal

infixl 1 ===
(===) = Equiv
