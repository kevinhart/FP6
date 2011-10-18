module Macros where

import SExpr

type Macro a = (a,a)
type Macros a = [Macro a]

substitute :: (Eq a, Show a) => Macros a -> SExpr a -> SExpr a
substitute [] exp = exp
substitute (x:xs) exp = substitute xs (sub x exp)
    where
        sub mac (Name a) = if a == (fst mac) then (Name (snd mac)) else (Name a)
        sub mac (Proc a se) = if a == (fst mac) then (Proc (snd mac) se) else (Proc a (sub  mac se))
        sub mac (Call sea seb) = (Call (sub mac sea) (sub mac seb))


{-
e1 = [ " (((lambda (x) (lambda (x) x)) a) b) ",
       " (((lambda (x) (lambda (y) x)) y) foo) " ]
       
m2 = [ " (define true  (lambda (x) (lambda (y) x))) ",
       " (define false (lambda (x) (lambda (y) y))) ",
       " (define if    (lambda (cond) (lambda (then) (lambda (else) ((cond then) else))))) " ]

e2 = [ " (((if true) this) that) " ,
       " (((if false) this) that) " ]
-}
