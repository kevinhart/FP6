module Testing where

import SExpr
import Macros

-- samples from the second assignment

--e1 = [ " (((lambda (\"x\") (lambda (\"x\") \"x\")) \"a\") \"b\") ",
--       " (((lambda (\"x\") (lambda (\"y\") \"x\")) \"y\") \"foo\") " ]
  
boolsm = [ " (define \"true\"  (lambda (\"x\") (lambda (\"y\") \"x\"))) ",
           " (define \"false\" (lambda (\"x\") (lambda (\"y\") \"y\"))) ",
           " (define \"if\"    (lambda (\"cond\") (lambda (\"then\") (lambda (\"else\") ((\"cond\" \"then\") \"else\"))))) " ]

boolstest = [ " (((\"if\" \"true\") \"this\") \"that\") " ,
              " (((\"if\" \"false\") \"this\") \"that\") " ]

-- 3. Church Numerals

cnumsm = [ "(define \"0\" (lambda (\"f\") (lambda (\"x\") \"x\")))",
           "(define \"1\" (lambda (\"f\") (lambda (\"x\") (\"f\" \"x\"))))",
           "(define \"2\" (lambda (\"f\") (lambda (\"x\") (\"f\" (\"f\" \"x\")))))",
           "(define \"3\" (lambda (\"f\") (lambda (\"x\") (\"f\" (\"f\" (\"f\" \"x\"))))))" ]

cnumstest = [ "((\"0\" (lambda (\"f\") (\"f\" \"hello\"))) (lambda (\"x\") \"x\"))",
              "((\"1\" (lambda (\"f\") (\"f\" \"hello\"))) (lambda (\"x\") \"x\"))",
              "((\"2\" (lambda (\"f\") (\"f\" \"hello\"))) (lambda (\"x\") \"x\"))",
              "((\"3\" (lambda (\"f\") (\"f\" \"hello\"))) (lambda (\"x\") \"x\"))" ]

-- 4. iteration

repeatm = [ "(define \"repeat\" (lambda (\"n\") (lambda (\"x\") ((\"n\" (lambda (\"g\") (\"g\" \"x\"))) (lambda (\"y\") \"y\")))))" ]

repeattest = [ "((\"repeat\" \"2\") \"hello\")" ]

-- 5. counting up and down

countm = [ "(define \"succ\" (lambda (\"n\") (lambda (\"f\") (lambda (\"x\") (\"f\" ((\"n\" \"f\") \"x\"))))))",
           "(define \"pred\" (lambda (\"n\") (((\"n\" (lambda (\"p\") (lambda (\"z\") ((\"z\" (\"succ\" (\"p\" \"true\"))) (\"p\" \"true\"))))) (lambda (\"z\") ((\"z\" \"0\") \"0\"))) \"false\")))" ]

counttest = [ "((\"repeat\" (\"succ\" \"2\")) \"hello\")",
              "((\"repeat\" (\"pred\" \"3\")) \"hello\")" ]
              
              
-- combined macros
macros = (map read (boolsm ++ cnumsm ++ repeatm ++ countm)) :: Macros String

-- example of how to get results of tests
countresult = map (beta . substitute macros . read) counttest :: [SExpr String]
