module Macros where

import SExpr

-- defines a Macro type as a key, value pair
data Macro a = Macro { macroKey :: a, macroValue :: SExpr a }

-- Macros is just a list of Macro
type Macros a = [Macro a]

-- substitute goes through an SExpr and attempts to replace Names with macros
-- if the Name matches a macro key; otherwise nothing is substituted
substitute :: (Eq a, Show a) => Macros a -> SExpr a -> SExpr a

-- base case; no macros so no substitution
substitute [] exp = exp

-- look at the first macro and attempt to substitute it, then recurse on the
-- rest
substitute (x:xs) exp = substitute xs (sub x exp)
  where
    -- test the name against the macro key and substitute the macro value
    -- on success
    sub mac (Name a) = if a == (macroKey mac) then (macroValue mac) else (Name a)
    
    -- induction on the body of a Proc and on each "part" of a Call
    sub mac (Proc a se) = Proc a (sub  mac se)
    sub mac (Call sea seb) = Call (sub mac sea) (sub mac seb)

-- implements Show for a Macro: "(define MACROKEY MACROVAL)"
showsMacro :: (Show a) => Macro a -> String -> String
showsMacro x = ("(define "++) . (shows (macroKey x)) . (" "++) . showsSExpr (macroValue x) . (')':)

instance (Show a) => Show (Macro a) where
  showsPrec _ x = showsMacro x

-- implements Read for a Macro: "(define MACROKEY MACROVAL)" read and creates
-- a (Macro MACROKEY MACROVAL)
readsMacro :: (Read a) => ReadS (Macro a)
readsMacro m = [(Macro key val, x) | ("(",      t) <- lex m,
                                     ("define", u) <- lex t,
                                     (key,      v) <- reads u,
                                     (val,      w) <- readsSExpr v,
                                     (")",      x) <- lex w]

instance (Read a) => Read (Macro a) where
  readsPrec _ x = readsMacro x
