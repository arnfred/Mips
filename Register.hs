module Register(adder, toInt, toSignedInt, padL, padR, readBits, showBits, Bits, Register) where

type Register	= Int
type Bits		= [Bit]
type Bit		= Bool

-- This is a binary adder, adding to bit strings.
adder :: Bits -> Bits -> Bits
adder rs ts = reverse $ take 32 $ adder' (reverse rs) (reverse ts) False
	where
		adder' [] [] n			= [n]
		adder' [] (t:ts) n		= (xor t n) : adder' [] ts (t && n)
		adder' (r:rs) [] n		= (xor r n) : adder' rs [] (r && n)
		adder' (r:rs) (t:ts) n	= d : adder' rs ts m
			where
				d	= (xor r t) `xor` n
				m	= (r && t) || ((xor r t) && n)

-- Converts a bit string to an unsigned integer
-- Not correct behaviour on empty string
toInt :: Bits -> Int
toInt [] = error "can't return number from empty bitlist"
toInt bs = sum . zipWith i p $ reverse bs
	where 
		p			= [2^n | n <- [0..]]
		i n True	= 1*n
		i _ False	= 0
		

-- Converts a bit string to a signed integer
-- Not correct behaviour on empty string
toSignedInt :: Bits -> Int
toSignedInt [] = 0
toSignedInt (b:bs)
	| b == True	= (*(-1)) $ (+1) $ toInt $ map not (b:bs)
	| otherwise	= toInt (b:bs)

-- Pads a register with '0' on the left side
padL :: Bit -> Bits -> Bits
padL b bs = (take (32 - length bs) $ repeat b) ++ bs

-- Pads a register with '0' on the right side
padR :: Bit -> Bits -> Bits
padR b bs = bs ++ (take (32 - length bs) $ repeat b)

-- True if b1 and b2 are different
xor :: Bool -> Bool -> Bool
xor b1 b2 = (not (b1 && b2)) &&  (b1 || b2)

readBits :: String -> Bits
readBits = map (\x -> if (x == '0') then False else True)

showBits :: Bits -> String
showBits = map (\x -> if x then '1' else '0')
