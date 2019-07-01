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
getNextInstruction i1
  = case i1 of 
      Move distance i2      -> i2
      Turn angle i2         -> i2
      SetStyle lineStyle i2 -> i2
      SetColour colour i2   -> i2
      PenDown i2            -> i2
      PenUp i2              -> i2


-- setter
setNextInstruction :: Instructions -> Instructions -> Instructions
setNextInstruction i1 i2 
  = case i1 of 
      Move distance _       -> Move distance i2
      Turn angle _          -> Turn angle i2
      SetStyle lineStyle _  -> SetStyle lineStyle i2
      SetColour colour _    -> SetColour colour i2
      PenDown _             -> PenDown i2
      PenUp _               -> PenUp i2


-- main functions:

andThen :: Instructions -> Instructions -> Instructions
andThen i1 i2 = case i1 of
 Stop -> i2
 _ -> case (getNextInstruction i1) of
    Stop -> setNextInstruction i1 i2 
    _    -> setNextInstruction i1 $ (getNextInstruction i1) `andThen` i2


loop :: Int -> Instructions -> Instructions
loop n i 
  | n <= 0 = Stop
  | otherwise = i `andThen` loop (n - 1) i


invisibly :: Instructions -> Instructions
invisibly i = PenUp $ invisibly_helper i True
 where 
 -- invisibly helper
 invisibly_helper :: Instructions -> Bool -> Instructions
 invisibly_helper i finalPenDown
  = case i of 
      Stop -> case finalPenDown of
          True  -> PenDown Stop
          False -> Stop
      PenDown i2  -> invisibly_helper i2 True
      PenUp i2    -> invisibly_helper i2 False
      _ -> setNextInstruction i $ invisibly_helper (getNextInstruction i) finalPenDown


retrace :: Instructions -> Instructions
retrace i = retrace_helper start i Stop
  where
   -- change the status of tortoise
   changeState :: TortoiseState -> Instructions -> TortoiseState
   changeState s i
    | i == Stop = s
    | otherwise = snd (tortoise (setNextInstruction i Stop) s)
   -- retrace helper
   retrace_helper :: TortoiseState -> Instructions -> Instructions -> Instructions
   retrace_helper curr_state i reversed_i
    | i == Stop = reversed_i
    | otherwise = retrace_helper new_state new_i new_reversed_i
      where
        new_state = changeState curr_state i
        new_i = getNextInstruction i
        new_reversed_i = setNextInstruction (reverseInstruction i) reversed_i
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
overlay is = case is of
  [] -> Stop
  (x:xs) -> (x `andThen` (invisibly (retrace x))) `andThen` (overlay xs)

