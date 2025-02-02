-- | Типы для языка Colon

module Types where

import Data.Map (Map)

-- | Слова
data Cmd
  = Word String  -- Слово (команда)
  | Number Int   -- Число
  | IfThen [Cmd] [Cmd]  -- IF ... THEN или IF ... ELSE ... THEN
  deriving (Eq, Show, Read)

-- | Программа
newtype Program = Program
  { getTokens :: [Cmd] }
  deriving (Eq,Show,Read)

-- | Словарь
newtype Dict = Dict
  { getDefinitions :: Map String Program }
  deriving (Eq,Show,Read)
