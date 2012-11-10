module Controller (run, step) where

import Memory

-- Runs a program in State
run :: MemState
run = do
		step	-- Execute one step
		-- Then find out if we are executing the next or ending the program
		end <- isEnd
		if (end) 
			then return () 	-- If we are at end of file, exit
			else run		-- Else, execute again

-- Runs one line of Program Code and updates the Program Counter
step :: MemState
step = do
		incPc						-- Increment the Program Counter
		pc			<- getIntPc		-- Get Counter
		len			<- getLength

		-- Set end flag if we are at end of program, or run instruction
		if (pc >= len) 
			then setEnd
			else runInstruction pc

-- Executes one instruction from the Program Code
runInstruction :: Int -> MemState
runInstruction pc = getInstruction pc >>= id
