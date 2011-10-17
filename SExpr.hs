module SExpr where

import Data.List (find)

data SExpr a = Name a | Proc a (SExpr a) | Call (SExpr a) (SExpr a) deriving Eq

------------
-- Part 2 --
------------
showsSExpr :: (Show a) => SExpr a -> String -> String
showsSExpr (Name a) = shows a
showsSExpr (Proc a se) = ("(lambda ("++) . shows a . (") "++) . showsSExpr se . (')':)
showsSExpr (Call sea seb) = ('(':) . showsSExpr sea . (' ':) . showsSExpr seb . (')':)

instance (Show a) => Show (SExpr a) where
  showsPrec _ x = showsSExpr x

readsSExpr :: (Read a) => ReadS (SExpr a)
readsSExpr s = [(Proc var expr, z) | ("(",      t) <- lex s,
                                     ("lambda", u) <- lex t,
                                     ("(",      v) <- lex u,
                                     (var,      w) <- reads v,
                                     (")",      x) <- lex w,
                                     (expr,     y) <- readsSExpr x,
                                     (")",      z) <- lex y         ]
               ++
               [(Call exp1 exp2, w) | ("(",  t) <- lex s,
                                      (exp1, u) <- readsSExpr t,
                                      (exp2, v) <- readsSExpr u,
                                      (")",  w) <- lex v         ]
               ++
               [(Name nym, t) | (nym, t) <- reads s]

instance (Read a) => Read (SExpr a) where
  readsPrec _ x = readsSExpr x

------------
-- Part 3 --
------------

-- Performs alpha-substitution on the given SExpr
alpha :: (Num n, Eq a) => SExpr a -> SExpr (n, a)
alpha expr = newexpr where (_, newexpr) = alphaR (0, expr) []

-- Recursive helper for alpha
alphaR :: (Num n, Eq a) => (n, SExpr a) -> [(n, a)] -> (n, SExpr (n, a))
{- If the SExpression is a name, look for the first pair on the stack that
   matches the name.  If found, that is the alpha-substituted name.  If one is
   not found, create a new pair and increment the next pair index counter.
-}
alphaR (n, Name nym) stack = case find (\(_,s) -> s == nym) stack of
                               Just p -> (n, Name p)
                               Nothing -> (n+1, Name (n, nym))
{- If the SExpression is a lambda, push a new pair with the argument onto the
   stack before alpha-substituting the contained expression.
-}
alphaR (n, Proc var expr) stack = (newn, Proc (n, var) newexp) where
                                    (newn, newexp) = alphaR (n+1, expr) ((n, var):stack)
{- If the SExpression is a call, just alpha-substitute on the two components
   and stick them together.
-}
alphaR (n, Call exp1 exp2) stack = let
                                     (n1, nexp1) = alphaR (n, exp1) stack
                                     (n2, nexp2) = alphaR (n1, exp2) stack
                                   in (n2, Call nexp1 nexp2)

-- Reverses the effect of alpha
unalpha :: SExpr (n, a) -> SExpr a
unalpha (Name (_, a)) = Name a
unalpha (Proc (_, var) expr) = Proc var (unalpha expr)
unalpha (Call exp1 exp2) = Call (unalpha exp1) (unalpha exp2)
