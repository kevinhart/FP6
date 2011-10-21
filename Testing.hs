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
              "((\"repeat\" (\"pred\" \"2\")) \"hello\")" ]
              
-- 6. arithmetic

arithm = [ "(define \"sum\" (lambda (\"m\") (lambda (\"n\") (lambda (\"f\") (lambda (\"x\") ((\"m\" \"f\") ((\"n\" \"f\") \"x\")))))))",
           "(define \"product\" (lambda (\"m\") (lambda (\"n\") (lambda (\"f\") (\"m\" (\"n\" \"f\"))))))" ]

arithtest = [ "((\"repeat\" ((\"sum\" \"2\") \"3\")) \"hello\")",
              "((\"repeat\" ((\"product\" \"2\") \"3\")) \"hello\")" ]              

-- 7. predicate

predm = [ "(define \"isZero\" (lambda (\"n\") ((\"n\" (lambda (\"x\") \"false\")) \"true\")))" ]

predtest = [ "(((\"if\" (\"isZero\" \"0\")) \"this\") \"that\")",
             "(((\"if\" (\"isZero\" \"1\")) \"this\") \"that\")" ]

-- 8. recursion

-- original definitions; NOTE these themselves contain macros
recurm = [ "(define \"r\" (lambda (\"f\") (lambda (\"n\") (((\"if\" (\"isZero\" \"n\")) \"1\") ((\"product\" \"n\") (\"f\" (\"pred\" \"n\")))))))",
               "(define \"Y\" (lambda (\"y\") ((lambda (\"x\") (\"y\" (\"x\" \"x\"))) (lambda (\"x\") (\"y\" (\"x\" \"x\"))))))",
               "(define \"factorial\" (\"Y\" \"r\"))" ]
               
recurtest = [ "((\"repeat\" (\"factorial\" ((\"product\" \"2\") \"2\"))) \"hello\")" ]


-- combined macros
macros = (map read (boolsm ++ cnumsm ++ repeatm ++ countm ++ arithm ++ predm ++ recurm)) :: Macros String

-- testresult reads a list of strings that are expressions, applies macro substitution,
-- then applies beta-reduction to produce a list of result SExpr String
testresult :: [String] -> [SExpr String]
testresult x = map (beta . substitute macros . read) x

-- example of how to get results of tests
countresult = testresult counttest
arithresult = testresult arithtest
predresult = testresult predtest
recurresult = testresult recurtest
