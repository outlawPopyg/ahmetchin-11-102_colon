module Main where

import Test.HUnit
import Interpreter (interpret, initialState, InterpreterState(..))
import Types (Program(..), Cmd(..))

testConditionals :: Test
testConditionals = TestList
  [ "IF-THEN (true)" ~: do
      let p = parse "1 IF 42 ELSE 100 THEN"
      s <- interpret p initialState
      stack s @?= [42]

  , "IF-ELSE-THEN (false)" ~: do
      let p = parse "0 IF 42 ELSE 100 THEN"
      s <- interpret p initialState
      stack s @?= [100]
  ]


allTests :: Test
allTests = TestList
  [ testConditionals
  ]

-- Запуск тестов
main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts + failures counts == 0
    then putStrLn "Все тесты прошли успешно!"
    else putStrLn "Тесты завершились с ошибками."