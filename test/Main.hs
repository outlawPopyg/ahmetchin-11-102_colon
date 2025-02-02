module Main where

import Test.HUnit
import Interpreter (interpret, initialState, InterpreterState(..))
import Types (Program(..), Cmd(..))

-- Вспомогательная функция для запуска программы и получения состояния стека
runProgram :: Program -> [Int]
runProgram program = stack finalState
  where
    (finalState, _) = interpret program initialState

-- Тесты для арифметических операций
testArithmetic :: Test
testArithmetic = TestList
  [ "1 2 3 +" ~: runProgram (Program [Number 1, Number 2, Number 3, Word "+"]) ~?= [1, 5]
  , "1 2 3 + +" ~: runProgram (Program [Number 1, Number 2, Number 3, Word "+", Word "+"]) ~?= [6]
  , "1 2 -" ~: runProgram (Program [Number 1, Number 2, Word "-"]) ~?= [-1]
  , "3 4 *" ~: runProgram (Program [Number 3, Number 4, Word "*"]) ~?= [12]
  , "4 5 /" ~: runProgram (Program [Number 4, Number 5, Word "/"]) ~?= [0]
  , "-12 5 MOD" ~: runProgram (Program [Number (-12), Number 5, Word "MOD"]) ~?= [-2]
  ]

-- Тесты для манипуляций со стеком
testStackManipulation :: Test
testStackManipulation = TestList
  [ "1 2 3 DUP" ~: runProgram (Program [Number 1, Number 2, Number 3, Word "DUP"]) ~?= [1, 2, 3, 3]
  , "1 2 3 DROP" ~: runProgram (Program [Number 1, Number 2, Number 3, Word "DROP"]) ~?= [1, 2]
  , "1 2 3 SWAP" ~: runProgram (Program [Number 1, Number 2, Number 3, Word "SWAP"]) ~?= [1, 3, 2]
  , "1 2 3 OVER" ~: runProgram (Program [Number 1, Number 2, Number 3, Word "OVER"]) ~?= [1, 2, 3, 2]
  , "1 2 3 ROT" ~: runProgram (Program [Number 1, Number 2, Number 3, Word "ROT"]) ~?= [2, 3, 1]
  ]

-- Тесты для обработки ошибок
testErrorHandling :: Test
testErrorHandling = TestList
  [ "1 + (Stack underflow)" ~: runProgram (Program [Number 1, Word "+"]) ~?= [1]
  , "1 2 3 + + + (Stack underflow)" ~: runProgram (Program [Number 1, Number 2, Number 3, Word "+", Word "+", Word "+"]) ~?= [6]
  , "4 0 / (Division by zero)" ~: runProgram (Program [Number 4, Number 0, Word "/"]) ~?= [4, 0]
  ]

-- Тесты для комментариев
testComments :: Test
testComments = TestList
  [ "( + 2 3 ) 1" ~: runProgram (Program [Word "(", Number 2, Number 3, Word "+", Word ")", Number 1]) ~?= [1]
  , "1 ( + 2 3 )" ~: runProgram (Program [Number 1, Word "(", Number 2, Number 3, Word "+", Word ")"]) ~?= [1]
  , "( + 2 3 ( - 3 2 ) ) 1" ~: runProgram (Program [Word "(", Number 2, Number 3, Word "+", Word "(", Number 3, Number 2, Word "-", Word ")", Word ")", Number 1]) ~?= [1]
  , "( n1 n2 -- сумма ) 1 2 +" ~: runProgram (Program [Word "(", Word "n1", Word "n2", Word "--", Word "сумма", Word ")", Number 1, Number 2, Word "+"]) ~?= [3]
  ]

-- Все тесты
allTests :: Test
allTests = TestList
  [ testArithmetic
  , testStackManipulation
  , testErrorHandling
  , testComments
  ]

-- Запуск тестов
main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts + failures counts == 0
    then putStrLn "Все тесты прошли успешно!"
    else putStrLn "Тесты завершились с ошибками."