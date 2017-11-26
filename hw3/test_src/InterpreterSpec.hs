module InterpreterSpec where

import           Structure
import           Interpreter
import           Test.Hspec
import qualified Data.Map as M

test_newVar :: Program
test_newVar =
  [ NewVarStatement "x" (Lit 1)
  ]

test_newVarTwice :: Program
test_newVarTwice =
  [ NewVarStatement "x" (Lit 1)
  , NewVarStatement "x" (Lit 2)
  ]

test_updVar :: Program
test_updVar =
  [ NewVarStatement "x" (Lit 0)
  , UpdVarStatement "x" (Lit 1)
  ]

test_updVarTwice :: Program
test_updVarTwice =
  [ NewVarStatement "x" (Lit 0)
  , UpdVarStatement "x" (Lit 1)
  , UpdVarStatement "x" (Lit 2)
  ]

test_updNoVar :: Program
test_updNoVar =
  [ UpdVarStatement "x" (Lit 1)
  ]

test_readVar :: Program
test_readVar =
  [ NewVarStatement "x" (Lit 0)
  , ReadStatement "x"
  ]

test_printVar :: Program
test_printVar =
  [ PrintStatement (Lit 10)
  ]

test_ifStmtT :: Program
test_ifStmtT =
  [ IfStatement (Lit 1) [PrintStatement (Lit 0)]
  ]

test_ifStmtF :: Program
test_ifStmtF =
  [ IfStatement (Lit 0) [PrintStatement (Lit 0)]
  ]

test_forStmt :: Program
test_forStmt =
  [ ForStatement "i" (Lit 0) (Lit 4) [PrintStatement (Var "i")]
  ]

test_breakMain :: Program
test_breakMain =
  [ PrintStatement (Lit 1)
  , BreakStatement
  , PrintStatement (Lit 2)
  ]

test_breakFor :: Program
test_breakFor =
  [ ForStatement "i" (Lit 0) (Lit 5)
    [ PrintStatement (Lit 1)
    , BreakStatement
    , PrintStatement (Lit 2)
    ]
  , PrintStatement (Lit 3)
  ]

test_factorial :: Program
test_factorial =
  [ NewVarStatement "x" (Lit 0)
  , ReadStatement "x"
  , NewVarStatement "res" (Lit 1)
  , ForStatement "i" (Lit 2) (Var "x")
    [ UpdVarStatement "res" (Var "res" `Mul` Var "i")
    ]
  , PrintStatement (Var "res")
  ]

type Result = (Either InterpreterErrorInfo Dictionary, [Int])

shouldPrints :: (HasCallStack) => Result -> [Int] -> Expectation
shouldPrints (_, a) e = reverse a `shouldBe` e

shouldHasInDictionary :: (HasCallStack) => Result -> [(Ref, Int)] -> Expectation
shouldHasInDictionary (Right a, _) e = M.toList a `shouldContain` e
shouldHasInDictionary (Left _,  _) _ = expectationFailure "run should succeed"

shouldFailsWith :: (HasCallStack) => Result -> InterpreterError -> Expectation
shouldFailsWith (Right _, _) _             = expectationFailure "run should failed"
shouldFailsWith (Left (ErrInfo _ a),  _) e = a `shouldBe` e

runTestWith :: Program -> [Int] -> Result
runTestWith = interpretList . programInterpreter

runTest :: Program -> Result
runTest = flip runTestWith []

spec :: Spec
spec = do
  describe "variable declare operation" $ do
    it "once"  $ runTest test_newVar `shouldHasInDictionary` [("x", 1)]
    it "twice" $ runTest test_newVarTwice `shouldFailsWith` VarRedefError "x"

  describe "variable assigment operation" $ do
    it "once"   $ runTest test_updVar `shouldHasInDictionary` [("x", 1)]
    it "twice"  $ runTest test_updVarTwice `shouldHasInDictionary` [("x", 2)]
    it "no var" $ runTest test_updNoVar `shouldFailsWith` NoVarUpdateError "x"

  describe "read operation" $
    it "read" $ runTestWith test_readVar [10] `shouldHasInDictionary` [("x", 10)]

  describe "print operation" $
    it "print" $ runTest test_printVar `shouldPrints` [10]

  describe "if statement" $ do
    it "true"  $ runTest test_ifStmtT `shouldPrints` [0]
    it "false" $ runTest test_ifStmtF `shouldPrints` []

  describe "for statement" $
    it "0..4" $ runTest test_forStmt `shouldPrints` [0,1,2,3,4]

  describe "break" $ do
    it "main" $ runTest test_breakMain `shouldPrints` [1]
    it "for"  $ runTest test_breakFor `shouldPrints` [1, 3]

  describe "factorial" $ do
    it "1" $ runTestWith test_factorial [1] `shouldPrints` [1]
    it "2" $ runTestWith test_factorial [2] `shouldPrints` [2]
    it "3" $ runTestWith test_factorial [3] `shouldPrints` [6]
    it "4" $ runTestWith test_factorial [4] `shouldPrints` [24]
