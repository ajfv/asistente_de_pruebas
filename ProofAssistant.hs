{-# LANGUAGE FlexibleInstances #-}
module ProofAssistant where

import Operators

data Substitute = Sub Term Term
(=:) = Sub

class Sust s where
    sust :: Term -> s -> Term

instance Sust Substitute where
    sust TrueTerm _ = TrueTerm
    sust FalseTerm _ = FalseTerm
    sust (Var x) (Sub t (Var p)) = if x == p then t else (Var x)
    sust (Not t) s = Not (sust t s)
    sust (Or t1 t2) s = Or (sust t1 s) (sust t2 s)
    sust (And t1 t2) s = And (sust t1 s) (sust t2 s) 
    sust (Imply t1 t2) s = And (sust t1 s) (sust t2 s) 
    sust (Equal t1 t2) s = Equal (sust t1 s) (sust t2 s) 
    sust (Unequal t1 t2) s = Unequal (sust t1 s) (sust t2 s) 

instance Sust (Term, Substitute, Term) where
    sust TrueTerm _ = TrueTerm
    sust FalseTerm _ = FalseTerm
    sust (Var x) (t1, Sub t2 (Var p), Var q)
        | p == x = t1
        | q == x = t2
        | otherwise = Var x
    sust (Not t) s = Not (sust t s)
    sust (Or t1 t2) s = Or (sust t1 s) (sust t2 s)
    sust (And t1 t2) s = And (sust t1 s) (sust t2 s) 
    sust (Imply t1 t2) s = And (sust t1 s) (sust t2 s) 
    sust (Equal t1 t2) s = Equal (sust t1 s) (sust t2 s) 
    sust (Unequal t1 t2) s = Unequal (sust t1 s) (sust t2 s)

instance Sust (Term, Term, Substitute, Term, Term) where
    sust TrueTerm _ = TrueTerm
    sust FalseTerm _ = FalseTerm
    sust (Var x) (t1, t2, Sub t3 (Var p), Var q, Var r)
        | p == x = t1
        | q == x = t2
        | r == x = t3
        | otherwise = Var x
    sust (Not t) s = Not (sust t s)
    sust (Or t1 t2) s = Or (sust t1 s) (sust t2 s)
    sust (And t1 t2) s = And (sust t1 s) (sust t2 s) 
    sust (Imply t1 t2) s = And (sust t1 s) (sust t2 s) 
    sust (Equal t1 t2) s = Equal (sust t1 s) (sust t2 s) 
    sust (Unequal t1 t2) s = Unequal (sust t1 s) (sust t2 s)
