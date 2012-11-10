module Instructions where

import Memory
import Register

-- Reserved Registers
m0 = 32
m1 = 33
m2 = 34
m3 = 35
				
------------
-- Logical
------------

-- And
andB :: Register -> Register -> Register -> MemState
andB rd rs rt	= get rt >>= andi rd rs


-- And with an immidiate register
andi :: Register -> Register -> Bits -> MemState
andi rd rs im	= do
				s <- get rs
				put rd (zipWith (&&) s (padL False im))
-- Or
orB :: Register -> Register -> Register -> MemState
orB rd rs rt	= get rt >>= ori rd rs


-- Bitwise or with immidiate value
ori :: Register -> Register -> Bits -> MemState
ori rd rs im	= do
				s <- get rs
				put rd (zipWith (&&) s (padL False im))

-- Not
nor :: Register -> Register -> Register -> MemState
nor rd rs rt	= do
				orB rd rs rt
				notB rd rd

-- Not || NOT MIPS INSTRUCTION!
notB :: Register -> Register -> MemState
notB rd rs		= do
				s <- get rs
				put rd (zipWith (\x y -> not x) s s)

-- Shift left by amount specified by sa
sll :: Register -> Register -> Bits -> MemState
sll rd rs sa	= do
				s <- get rs
				put rd $ padR False $ drop (toInt sa) s
				 
-- Shift left by the amount in a register
sllv :: Register -> Register -> Register -> MemState
sllv rd rs rt	= do
				andi rd rt $ readBits "11111"
				get rd >>= sll rd rs

-- Shift right by amount specified by sa
srl :: Register -> Register -> Bits -> MemState
srl rd rs sa	= do
				s <- get rs
				put rd $ padL False $ take (32 - (toInt sa)) s

-- Shift right by the amount in a register
srlv :: Register -> Register -> Register -> MemState
srlv rd rs rt	= do
				andi rd rt $ readBits "11111"
				get rd >>= srl rd rs

-- Shift right by amount specified by sa
sra :: Register -> Register -> Bits -> MemState
sra rd rs sa	= do
				s <- get rs
				put rd $ padL (head s) $ take (32 - (toInt sa)) s



-- Or exlusive with two registers
xor :: Register -> Register -> Register -> MemState
xor rd rs rt	= get rt >>= xori rd rs


-- Xor with immidiate value
xori :: Register -> Register -> Bits -> MemState
xori rd rs im	= do
				ori rd rs im
				andi m0 rs im
				notB m0 m0
				andB rd m0 rd

-----------
-- Jumps --
-----------

-- Jump to immidiate address
j :: Bits -> MemState
j b	= setPc b

-- Jump to address in register
jr :: Register -> MemState
jr r = get r >>= j

-- Jump and Link
jal :: Bits -> MemState
jal b = do
		pc <- getPc
		put 31 pc
		j b


-----------------
-- Comparisons --
-----------------

-- Set on less than signed
slt :: Register -> Register -> Register
slt rd rs rt	= get rt >>= slti rd rs

-- Set on less than unsigned
sltu :: Register -> Register -> Register
sltu rd rs rt	= get rt >>= sltiu rd rs

-- Set on less than signed immidiate
slti :: Register -> Register -> Register
slti rd rs im	= do
				s <- get rs
				if ((toSignedInt s) < (toSignedInt im))
					then put rd True
					else put rd False

-- Set on less than unsigned immidiate
sltiu :: Register -> Register -> Register
sltiu rd rs im	= do
				s <- get rs
				if ((toInt s) < (toInt im))
					then put rd True
					else put rd False

--------------
-- Branches --
--------------

-- Compare two registers and jump to relative address after delay
beq :: Register -> Register -> Bits -> MemState
beq rs rt offs	= do
				s <- get rs
				t <- get rt
				if (s == t)
					then setPc offs
					else return()

b :: Bits -> MemState
b offs			= beq 0 0 offs

bgez :: Register -> Bits -> MemState
bgez rs offs	= do
				slt m0 0 rs
				addiu m1 0 $ readBits "1"
				beq m0 m1 offs

bgtz :: Register -> Bits -> MemState
bgtz rs offs	= do
				neg m0 rs
				bltz m0 offs
				

bltz :: Register -> Bits -> MemState
bltz rs offs	= do
				slt m0 rs 0
				addiu m1 0 $ readBits "1"
				beq m0 m1 offs

blez :: Register -> Bits -> MemState
blez rs offs	= do
				neg m0 rs
				bgez m0 offs

bne :: Register -> Register -> MemState
bne rs rt offs	= do
				slt m0 rs rt
				slt m1 rt rs
				addu m0 m0 m1
				addiu m1 0 $ readBits "10"
				beq m0 m1 offs


------------------------
-- Integer Arithmetic --
------------------------

-- Adds two integers, throwing an overflow exception
--add rd rs rt	= do
--				andi m1 rs (padR False [True])
--				andi m2 rt (padR False [True])


-- Adds two integers without throwing an exception
addu rd rs rt	= do
				s <- get rs
				t <- get rt
				put rd (adder s t)

addiu rd rs im	= do
				s <- get rs
				put rd (adder s (padL False im))

-- Negates a signed bitstring (this is not in the mips instruction set)
neg rd rs		= do
				notB rd rs
				addu rd rd $ readBits "1"

-- Subtracts two registers
subu rd rs rt	= do
				neg m0 rt
				addu rd m0 rs
				
				
				
				

-- Two's complement arithmetic addition with overflow exception
--add :: Address -> Address -> Address -> S.State Memory ()
--add rd rs1 rs2 = 	do put result rd

-- Check for overflow

-- Add the to signed numbers
-- adder :: (Int



