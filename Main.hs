module Main where

import Memory
import Register
import Controller
import Instructions
import Data.Array


-- Runs through the program a step at a time, printing the memory for each step
runStep :: Instructions -> IO ()
runStep code 	= do
		putStrLn "Running program..."
		loop $ initMem code
	where
		loop mem = do
			writeOut $ mem
			putStrLn "..."
			q <- getChar
			mem' <- return $ snd $ runState step $ mem
			if (q == 'q' || (end mem')) 
				then return () 
				else loop mem'

-- Runs through the entire program and prints the memory in the end
runAll :: Instructions -> IO ()
runAll code = writeOut $ snd $ runState run $ initMem code
			
-------------
-- Testing --
-------------
testProg = listArray (0, length code) code
	where code = [ -- The program code
		put 10 (readBits "1111"), -- 13
		put 11 (readBits "0001"), -- 8
		andB 12 10 11,
		sll 12 12 (readBits "00001"),
		xor 13 10 11,
		xor 14 12 13]


