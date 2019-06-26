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


-- two helper functions:
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


getFinalStyle :: Instructions -> LineStyle
getFinalStyle i = style $ snd (tortoise i start)


getFinalColour :: Instructions -> Colour
getFinalColour i = colour $ snd (tortoise i start)



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
      PenUp i2 -> PenUp $ invisibly_helper i2 False
      Stop -> Stop
      _  -> PenUp $ invisibly_helper i True 


invisibly_helper :: Instructions -> Bool -> Instructions
invisibly_helper i shouldDown
  = case i of 
      PenDown i2 -> PenUp $ invisibly_helper i2 True
      PenUp i2 -> PenUp $ invisibly_helper i2 False
      Stop -> case shouldDown of
                True -> PenDown Stop
                False -> Stop
      _ -> setNextInstruction i $ invisibly_helper (getNextInstruction i) shouldDown


retrace :: Instructions -> Instructions
retrace i 
  | i == Stop = Stop
  | otherwise = retrace_helper start i Stop


-- if Stop, then Stop
-- if Move, Trun, reverse it
-- if PenUp, then PenDown, and ...
-- if 

changeState :: TortoiseState -> Instructions -> TortoiseState
changeState s i
  | i == Stop = s
  | otherwise = snd (tortoise i_once s)
    where i_once = setNextInstruction i Stop


retrace_helper :: TortoiseState -> Instructions -> Instructions -> Instructions
retrace_helper curr_state toreverse_i reversed_i
  | toreverse_i == Stop = reversed_i
  | otherwise = retrace_helper new_state new_toreverse_i new_reversed_i
    where
      new_state = changeState curr_state toreverse_i
      new_toreverse_i = getNextInstruction toreverse_i
      new_reversed_i = setNextInstruction (reverseInstruction toreverse_i) reversed_i
      reverseInstruction :: Instructions -> Instructions
      reverseInstruction i = case i of 
          Move distance instructions -> Move (-distance) instructions
          Turn angle instructions -> Turn (-angle) instructions
          PenUp instructions
            | penDown curr_state -> PenDown instructions
            | otherwise -> PenUp instructions
          PenDown instructions
            | penDown curr_state -> PenDown instructions
            | otherwise -> PenUp instructions
          SetStyle lineStyle instructions -> SetStyle (style curr_state) instructions
          SetColour lineColour instructions -> SetColour (colour curr_state) instructions


overlay :: [Instructions] -> Instructions
overlay is = error "'overlay' unimplemented"

