module SExpr where

data SExpr a = Name a | Proc a (SExpr a) | Call (SExpr a) (SExpr a)

showsSExpr :: (Show a) => SExpr a -> String -> String
showsSExpr (Name a) = shows a
showsSExpr (Proc a se) = ("(lambda ("++) . shows a . (") "++) . showsSExpr se . (')':)
showsSExpr (Call sea seb) = ('(':) . showsSExpr sea . (' ':) . showsSExpr seb . (')':)

instance (Show a) => Show (SExpr a) where
  showsPrec _ x = showsSExpr x
