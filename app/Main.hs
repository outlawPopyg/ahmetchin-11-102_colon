{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Data.Map (Map, empty, insert, lookup)
import Control.Monad (when, foldM)
import Data.Char (chr, ord)
import System.IO (hFlush, stdout)


-- Состояние интерпретатора
data InterpreterState = InterpreterState
  { stack :: [Int]              -- Стек для хранения данных
  , dictionary :: Dict          -- Словарь для хранения пользовательских определений
  , memory :: Map String Int    -- Память для хранения переменных
  , message :: String           -- Сообщение об ошибке или "ok"
  }

-- Начальное состояние интерпретатора
initialState :: InterpreterState
initialState = InterpreterState
  { stack = []              -- Пустой стек
  , dictionary = Dict empty -- Пустой словарь
  , memory = empty          -- Пустая память
  , message = "ok"          -- Начальное сообщение
  }


parse :: String -> Program
parse input = Program $ parseTokens (words input) []

parseTokens :: [String] -> [Cmd] -> [Cmd]
parseTokens [] acc = reverse acc
parseTokens ("IF":ts) acc =
  let (thenPart, rest1) = parseUntil ["ELSE", "THEN"] ts []
      (elsePart, rest2) = case rest1 of
        "ELSE":rs -> parseUntil ["THEN"] rs []
        _         -> ([], rest1)
  in parseTokens rest2 (If (parseTokens thenPart []) (parseTokens elsePart []) : acc)
parseTokens (t:ts) acc
  | all (\c -> c == '-' || c `elem` ['0'..'9']) t =
      parseTokens ts (Number (read t) : acc)
  | otherwise = parseTokens ts (Word t : acc)

parseUntil :: [String] -> [String] -> [String] -> ([String], [String])
parseUntil stopWords ts acc = case ts of
  [] -> (reverse acc, [])
  t:rest
    | t `elem` stopWords -> (reverse acc, rest)
    | otherwise -> parseUntil stopWords rest (t:acc)


execute :: Cmd -> InterpreterState -> IO InterpreterState
execute (Number n) state =
  pure $ state { stack = n : stack state, message = "ok" }

execute (Word "DUP") state@InterpreterState{stack = s}
  | null s    = pure $ state { message = "Stack underflow" }
  | otherwise = pure $ state { stack = head s : s, message = "ok" }

execute (Word "DROP") state@InterpreterState{stack = s}
  | null s    = pure $ state { message = "Stack underflow" }
  | otherwise = pure $ state { stack = tail s, message = "ok" }

execute (If thenCmds elseCmds) state@InterpreterState{stack = s} = case s of
  (0:rest)  -> interpret (Program elseCmds) state { stack = rest }
  (_:rest)  -> interpret (Program thenCmds) state { stack = rest }
  []        -> pure $ state { message = "Stack underflow" }

execute (Word op) state = case op of
  "+" -> binaryOp (+) state
  "-" -> binaryOp (-) state
  "*" -> binaryOp (*) state
  "/" -> binaryDiv state
  "=" -> compareOp (==) state
  "<" -> compareOp (<) state
  ">" -> compareOp (>) state
  "AND" -> logicOp (&&) state
  "OR"  -> logicOp (||) state
  "INVERT" -> invertOp state
  ".EMIT" -> emitOp state
  _    -> pure $ state { message = "Unknown word: " ++ op }

interpret :: Program -> InterpreterState -> IO InterpreterState
interpret (Program cmds) state = do
  (finalState, _) <- foldM step (state, False) cmds
  putStrLn $ "> " ++ message finalState
  putStrLn $ "| " ++ showStack (stack finalState)
  return finalState
  where
    step (s, err) cmd
      | err       = pure (s, err)
      | otherwise = do
          newS <- execute cmd s
          pure $ if message newS == "ok"
            then (newS, False)
            else (newS { stack = stack s }, True)

-- Вспомогательные функции
binaryOp :: (Int -> Int -> Int) -> InterpreterState -> IO InterpreterState
binaryOp f state@InterpreterState{stack = a:b:s} =
  pure $ state { stack = f b a : s, message = "ok" }
binaryOp _ state = pure $ state { message = "Stack underflow" }

binaryDiv :: InterpreterState -> IO InterpreterState
binaryDiv state@InterpreterState{stack = a:b:s}
  | a == 0    = pure $ state { message = "Division by zero" }
  | otherwise = pure $ state { stack = b `div` a : s, message = "ok" }
binaryDiv state = pure $ state { message = "Stack underflow" }

compareOp :: (Int -> Int -> Bool) -> InterpreterState -> IO InterpreterState
compareOp f state@InterpreterState{stack = a:b:s} =
  pure $ state { stack = (if f b a then -1 else 0) : s, message = "ok" }
compareOp _ state = pure $ state { message = "Stack underflow" }

logicOp :: (Bool -> Bool -> Bool) -> InterpreterState -> IO InterpreterState
logicOp f state@InterpreterState{stack = a:b:s} =
  pure $ state { stack = (if f (b /= 0) (a /= 0) then -1 else 0) : s, message = "ok" }
logicOp _ state = pure $ state { message = "Stack underflow" }

invertOp :: InterpreterState -> IO InterpreterState
invertOp state@InterpreterState{stack = a:s} =
  pure $ state { stack = (if a == 0 then -1 else 0) : s, message = "ok" }
invertOp state = pure $ state { message = "Stack underflow" }

emitOp :: InterpreterState -> IO InterpreterState
emitOp state@InterpreterState{stack = a:s} = do
  putChar (chr a)
  pure $ state { stack = s, message = "ok" }
emitOp state = pure $ state { message = "Stack underflow" }

showStack :: [Int] -> String
showStack s = unwords (map show s) ++ " <- Top"

main :: IO ()
main = do
  let code = "5 0 > IF 42 ELSE 100 THEN .EMIT"
  let program = parse code
  _ <- interpret program initialState
  return ()