module Hex (HexString (..), BinaryString (..), fromBinary, toBinary) where

newtype HexString = HexString String
  deriving (Eq, Ord)

newtype BinaryString = BinaryString String
  deriving (Eq, Ord)

toBinary :: HexString -> BinaryString
toBinary (HexString s) = BinaryString $ concatMap charToBinary s

charToBinary :: Char -> String
charToBinary '0' = "0000"
charToBinary '1' = "0001"
charToBinary '2' = "0010"
charToBinary '3' = "0011"
charToBinary '4' = "0100"
charToBinary '5' = "0101"
charToBinary '6' = "0110"
charToBinary '7' = "0111"
charToBinary '8' = "1000"
charToBinary '9' = "1001"
charToBinary 'A' = "1010"
charToBinary 'B' = "1011"
charToBinary 'C' = "1100"
charToBinary 'D' = "1101"
charToBinary 'E' = "1110"
charToBinary 'F' = "1111"
charToBinary c = error "Invalid hex char: " ++ [c]

fromBinary :: String -> Int
fromBinary bs = sum $ do
  (b, e) <- zip (reverse bs) ([0..] :: [Int])
  return $ if b == '0' then 0 else 2^e
