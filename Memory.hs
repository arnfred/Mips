module Memory where

import Data.Array
import Register
import qualified Control.Monad.State as S


------------
-- Memory --
------------
data Memory = Memory {	reg	:: Registers,
						pc	:: Pc,
						end	:: Bool,
						ins	:: Instructions }

--------------
-- MemState --
--------------
type Mem			= S.State Memory 
type MemState		= Mem ()

-- Exporting RunState
runState = S.runState


------------------
-- Instructions --
------------------
type Instructions = Array Int MemState

-- Returns an Instruction from the Memory
getInstruction :: Int -> Mem MemState
getInstruction n = S.get >>= return . (!n) . ins

-- The number of Instructions
getLength :: Mem Int
getLength = S.get >>= return . snd . bounds . ins


---------------------
-- Program Counter --
---------------------
type Pc = (Bits,Bits)

-- Returns the counter from State
getPc :: Mem Bits
getPc = S.get >>= return . fst . pc

-- Modifies the counter in state
setPc :: Bits -> MemState
setPc npc = S.get >>= \m -> S.put $ m{pc = (fst $ pc m,npc)}

incPc :: MemState
incPc = S.get >>= \m -> S.put $ (\(_,npc) -> m{pc = (npc,adder npc [True])}) $ pc m

getIntPc :: Mem Int
getIntPc = getPc >>= return . toInt


--------------------
-- End of Program --
--------------------
isEnd :: Mem Bool
isEnd = S.get >>= return . end

setEnd :: MemState
setEnd = S.get >>= \m -> S.put $ m{end = True}

---------------
-- Registers -- 
---------------
type Registers		= Array Int Bits

-- Get a register from State
get :: Register -> Mem Bits
get r = S.get >>= return . (!r) . reg

-- Put a new register to State
put :: Register -> Bits -> MemState
put r b = S.get >>= \m -> S.put m{reg = writeMemory (reg m) r b}

-- Initial empty Array of registers
initMem :: Instructions -> Memory
initMem code = Memory reg (pc,pc) False code
	where
		reg		= mem//[(0, padL False [])]
		mem		= listArray (0,35) $ repeat []
		pc		= padL False []

-- Writes some bits to a specific register in the memory and then returns memory
writeMemory :: Registers -> Register -> Bits -> Registers
writeMemory m a b
	| a == 0	= m
	| otherwise	= m//[(a,sanitize b)]

-- Sanitizes a sequence of Bits
sanitize :: Bits -> Bits
sanitize = padL False  . take 32


------------------
-- Pretty Print --
------------------
regList :: Registers -> [Bits]
regList reg = map (reg!) [0..31]

writeOut :: Memory -> IO ()
writeOut m = do
		putStrLn $ header
		putStrLn $ regTable
	where
		(index,r)		= (\x -> (map fst x, map snd x)) $ filter (not . null . snd) $ zip [1..] $ take 32 $ regList $ reg m
		header 			= "\tPC: \t" ++ show (toInt $ fst $ pc m) ++ "\tNext PC:\t" ++ show (toInt $ snd $ pc m)
		regTable		= tableHead ++ tableBody
		tableHead		= "\t #\tBit String\t\t\t\tInt\tSigned Int"
		tableBody		= foldr (++) "" $ map (\(n,(a,b,c)) -> "\n\t " ++ show n ++ "\t" ++ a ++ "\t" ++ show b ++ "\t" ++ show c) $ zip index tableLine
		tableLine		= zip3 (map showBits $ r) (map toInt $ r) (map toSignedInt $ r)
