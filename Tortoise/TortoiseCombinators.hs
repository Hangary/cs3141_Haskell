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

andThen :: Instructions -> Instructions -> Instructions
andThen i1 i2 = error "'andThen' unimplemented"

loop :: Int -> Instructions -> Instructions
loop n i = error "'loop' unimplemented"

invisibly :: Instructions -> Instructions
invisibly i = error "'invisibly' unimplemented"

retrace :: Instructions -> Instructions
retrace i = error "'retrace' unimplemented"

overlay :: [Instructions] -> Instructions
overlay is = error "'overlay' unimplemented"

