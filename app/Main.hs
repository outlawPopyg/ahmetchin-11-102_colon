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

-- Функция для вывода состояния стека
showStack :: [Int] -> String
showStack stack = "| " ++ unwords (map show stack) ++ " <- Top"

-- Функция для выполнения команд
execute :: Cmd -> InterpreterState -> IO InterpreterState
execute (Number n) state@(InterpreterState stack dict mem _) =
  return $ state { stack = n : stack, message = "ok" }

execute (Word "DUP") state@(InterpreterState stack dict mem _)
  | length stack < 1 = return $ state { message = "Stack underflow" }
  | otherwise = return $ state { stack = head stack : stack, message = "ok" }

execute (Word "DROP") state@(InterpreterState stack dict mem _)
  | length stack < 1 = return $ state { message = "Stack underflow" }
  | otherwise = return $ state { stack = tail stack, message = "ok" }

execute (Word "SWAP") state@(InterpreterState stack dict mem _)
  | length stack < 2 = return $ state { message = "Stack underflow" }
  | otherwise = return $ state { stack = swap stack, message = "ok" }
  where
    swap (x:y:xs) = y : x : xs

execute (Word "OVER") state@(InterpreterState stack dict mem _)
  | length stack < 2 = return $ state { message = "Stack underflow" }
  | otherwise = return $ state { stack = over stack, message = "ok" }
  where
    over (x:y:xs) = y : x : y : xs

execute (Word "ROT") state@(InterpreterState stack dict mem _)
  | length stack < 3 = return $ state { message = "Stack underflow" }
  | otherwise = return $ state { stack = rot stack, message = "ok" }
  where
    rot (x:y:z:xs) = y : z : x : xs

execute (Word "+") state@(InterpreterState stack dict mem _)
  | length stack < 2 = return $ state { message = "Stack underflow" }
  | otherwise = return $ state { stack = (y + x) : xs, message = "ok" }
  where
    (x:y:xs) = stack

execute (Word "-") state@(InterpreterState stack dict mem _)
  | length stack < 2 = return $ state { message = "Stack underflow" }
  | otherwise = return $ state { stack = (y - x) : xs, message = "ok" }
  where
    (x:y:xs) = stack

execute (Word "*") state@(InterpreterState stack dict mem _)
  | length stack < 2 = return $ state { message = "Stack underflow" }
  | otherwise = return $ state { stack = (y * x) : xs, message = "ok" }
  where
    (x:y:xs) = stack

execute (Word "/") state@(InterpreterState stack dict mem _)
  | length stack < 2 = return $ state { message = "Stack underflow" }
  | x == 0 = return $ state { message = "Division by zero" }
  | otherwise = return $ state { stack = (y `div` x) : xs, message = "ok" }
  where
    (x:y:xs) = stack

execute (Word "MOD") state@(InterpreterState stack dict mem _)
  | length stack < 2 = return $ state { message = "Stack underflow" }
  | x == 0 = return $ state { message = "Division by zero" }
  | otherwise = return $ state { stack = (y `mod` x) : xs, message = "ok" }
  where
    (x:y:xs) = stack

execute (Word ".") state@(InterpreterState stack dict mem _)
  | length stack < 1 = return $ state { message = "Stack underflow" }
  | otherwise = do
      putStr (show (head stack) ++ " ")
      hFlush stdout
      return $ state { stack = tail stack, message = "ok" }

execute (Word "CR") state = do
  putStrLn ""
  return $ state { message = "ok" }

execute (Word "EMIT") state@(InterpreterState stack dict mem _)
  | length stack < 1 = return $ state { message = "Stack underflow" }
  | otherwise = do
      putChar (chr (head stack))
      return $ state { stack = tail stack, message = "ok" }

execute (Word "KEY") state@(InterpreterState stack dict mem _) = do
  c <- getChar
  return $ state { stack = ord c : stack, message = "ok" }

execute (Word word) state@(InterpreterState stack (Dict dict) mem _) =
  case Data.Map.lookup word dict of
    Just (Program cmds) -> foldM (flip execute) state cmds
    Nothing -> do
      return $ state { message = word ++ " ?" }

-- Функция для интерпретации программы
interpret :: Program -> InterpreterState -> IO InterpreterState
interpret (Program cmds) state = do
  (finalState, hadError) <- foldM (\(s, err) cmd ->
    if err
      then return (s, err)
      else do
        newS <- execute cmd s
        if message newS /= "ok"
          then return (newS, True)
          else return (newS, False)
    ) (state, False) cmds
  putStrLn $ "> " ++ message finalState
  putStrLn $ showStack (stack finalState)
  return finalState

main :: IO ()
main = do
  let program1 = Program [Number 1, Word "-"]
  let program2 = Program [Number 1, Number 2, Number 3, Word "+", Word "+"]
  let program3 = Program [Number 1, Number 2, Number 3, Word "*", Word "*", Word "*"]
  let program4 = Program [Number 1, Number 2, Word "-"]
  let program5 = Program [Number 3, Number 4, Word "*"]
  let program6 = Program [Number 4, Number 5, Word "/"]
  let program7 = Program [Number (-12), Number 5, Word "MOD"]
  let program8 = Program [Number 5, Number 2, Word "+", Number 10, Word "*"]

  initialState <- interpret program1 initialState
--  initialState <- interpret program2 initialState
--  initialState <- interpret program3 initialState
--  initialState <- interpret program4 initialState
--  initialState <- interpret program5 initialState
--  initialState <- interpret program6 initialState
--  initialState <- interpret program7 initialState
--  initialState <- interpret program8 initialState
  return ()