module Macros where

import SExpr

data Macro a = Macro { macroKey :: a, macroValue :: SExpr a }
type Macros a = [Macro a]

substitute :: (Eq a, Show a) => Macros a -> SExpr a -> SExpr a
substitute [] exp = exp
substitute (x:xs) exp = substitute xs (sub x exp)
  where
    sub mac (Name a) = if a == (macroKey mac) then (macroValue mac) else (Name a)
    sub mac (Proc a se) = Proc a (sub  mac se)
    sub mac (Call sea seb) = Call (sub mac sea) (sub mac seb)

showsMacro :: (Show a) => Macro a -> String -> String
showsMacro x = ("(define "++) . (shows (macroKey x)) . (" "++) . showsSExpr (macroValue x) . (')':)
--showsSExpr (Call sea seb) = ('(':) . showsSExpr sea . (' ':) . showsSExpr seb . (')':)

instance (Show a) => Show (Macro a) where
  showsPrec _ x = showsMacro x

readsMacro :: (Read a) => ReadS (Macro a)
readsMacro m = [(Macro key val, x) | ("(",      t) <- lex m,
                                     ("define", u) <- lex t,
                                     (key,      v) <- reads u,
                                     (val,      w) <- readsSExpr v,
                                     (")",      x) <- lex w]

instance (Read a) => Read (Macro a) where
  readsPrec _ x = readsMacro x

e1 = [ " (((lambda (\"x\") (lambda (\"x\") \"x\")) \"a\") \"b\") ",
       " (((lambda (x) (lambda (y) x)) y) foo) " ]
       
m2 = [ " (define \"true\"  (lambda (\"x\") (lambda (\"y\") \"x\"))) ",
       " (define \"false\" (lambda (\"x\") (lambda (\"y\") \"y\"))) ",
       " (define \"if\"    (lambda (\"cond\") (lambda (\"then\") (lambda (\"else\") ((\"cond\" \"then\") \"else\"))))) " ]

e2 = [ " (((if true) this) that) " ,
       " (((if false) this) that) " ]
