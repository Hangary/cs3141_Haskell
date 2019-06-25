module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.


-- two helper functions

-- getter
getNextInstruction :: Instructions -> Instructions
getNextInstruction instructions_1
    = case instructions_1 of 
          Move distance instructions_2 -> instructions_2
          Turn angle instructions_2 -> instructions_2
          SetStyle lineStyle instructions_2 -> instructions_2
          SetColour colour instructions_2 -> instructions_2
          PenDown instructions_2 -> instructions_2
          PenUp instructions_2 -> instructions_2

-- setter
setNextInstruction :: Instructions -> Instructions -> Instructions
setNextInstruction instructions_1 instructions_2 
    = case instructions_1 of 
          Move distance instructions -> Move distance instructions_2
          Turn angle instructions -> Turn angle instructions_2
          SetStyle lineStyle instructions -> SetStyle lineStyle instructions_2
          SetColour colour instructions -> SetColour colour instructions_2
          PenDown instructions -> PenDown instructions_2
          PenUp instructions -> PenUp instructions_2


andThen :: Instructions -> Instructions -> Instructions
andThen Stop instructions_2 = instructions_2
andThen instructions_1 Stop = instructions_1
andThen instructions_1 instructions_2 
    = case (getNextInstruction instructions_1) of
          Stop -> setNextInstruction instructions_1 instructions_2 
          _    -> setNextInstruction instructions_1 $  (getNextInstruction instructions_1) `andThen` instructions_2
      
          

loop :: Int -> Instructions -> Instructions
loop n i 
  | n <= 0 = Stop
  | otherwise = i `andThen` loop (n - 1) i

invisibly :: Instructions -> Instructions
invisibly i 
  = case i of 
      Stop -> Stop
      PenDown i2 -> PenUp $ invisibly i2
      _ -> setNextInstruction i $ invisibly $ getNextInstruction i


retrace :: Instructions -> Instructions
retrace i = error "'retrace' unimplemented"

overlay :: [Instructions] -> Instructions
overlay is = error "'overlay' unimplemented"

