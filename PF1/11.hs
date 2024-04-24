

decAHexHelper :: Int -> Char
decAHexHelper 0 = '0'
decAHexHelper 1 = '1'
decAHexHelper 2 = '2'
decAHexHelper 3 = '3'
decAHexHelper 4 = '4'
decAHexHelper 5 = '5'
decAHexHelper 6 = '6'
decAHexHelper 7 = '7'
decAHexHelper 8 = '8'
decAHexHelper 9 = '9'
decAHexHelper 10 = 'A'
decAHexHelper 11 = 'B'
decAHexHelper 12 = 'C'
decAHexHelper 13 = 'D'
decAHexHelper 14 = 'E'
decAHexHelper 15 = 'F'

decAHex :: Int -> [Char]
decAHex 0 = []
decAHex n =  decAHexHelper (n `mod` 16) : decAHex (n `div` 16)
