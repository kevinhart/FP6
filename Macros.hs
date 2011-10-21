module Macros where

import SExpr
import Data.List (find)

-- defines a Macro type as a key, value pair
data Macro a = Macro { macroKey :: a, macroValue :: SExpr a }

-- Macros is just a list of Macro
type Macros a = [Macro a]

-- substitute goes through an SExpr and attempts to replace Names with macros
-- if the Name matches a macro key; otherwise nothing is substituted
substitute :: (Eq a, Show a) => Macros a -> SExpr a -> SExpr a
-- perform structural induction until you find a Name
substitute macros (Proc v body) = Proc v (substitute macros body)
substitute macros (Call ex1 ex2) = Call (substitute macros ex1) (substitute macros ex2)
-- Once you find a Name, look for a macro that matches the contained name
substitute macros (Name nym) = case find (\m -> (macroKey m) == nym) macros of
  -- If you found one, return the value of the macro after substituting in it.
  Just m -> substitute macros (macroValue m)
  -- If you didn't find a matching macro, don't change anything
  Nothing -> Name nym

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
