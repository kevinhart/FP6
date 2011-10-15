module SExpr where

data SExpr a = Name a | Proc a (SExpr a) | Call (SExpr a) (SExpr a) deriving Eq

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
