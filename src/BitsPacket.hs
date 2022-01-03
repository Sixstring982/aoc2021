module BitsPacket (BitsPacket (..), parsePacket, evaluatePacket) where

import Emitter (Emitter, emit, emitted, evalEmitter, getCounter)
import Hex (BinaryString (..), HexString (..), fromBinary, toBinary)

--
-- Packets
--

data BitsPacket = Literal Int Int | Operator Int Int [BitsPacket]
  deriving (Eq, Show)

data LengthType = LengthBits | LengthPackets

--
-- Parsing
--

lengthTypeFromBit :: Char -> LengthType
lengthTypeFromBit '0' = LengthBits
lengthTypeFromBit '1' = LengthPackets
lengthTypeFromBit x   = error $ "Unknown length type: " ++ [x]

parseLiteralBits :: Emitter Char String
parseLiteralBits = do
  isLast <- (== "0") <$> emit 1
  byte <- emit 4
  if isLast
    then return byte
    else do
      nextBits <- parseLiteralBits
      return $ byte ++ nextBits

parseLiteral :: Int -> Emitter Char BitsPacket
parseLiteral version = Literal version <$> fromBinary <$> parseLiteralBits

parseOperator :: Int -> Int -> Emitter Char BitsPacket
parseOperator version typeId = do
  lengthType <- lengthTypeFromBit <$> head <$> emit 1
  case lengthType of
    LengthPackets -> do
      numPackets <- fromBinary <$> emit 11
      packets <- parseBitsPackets numPackets
      return $ Operator version typeId packets

    LengthBits -> do
      numBits <- fromBinary <$> emit 15
      packets <- parseBitsPacketBitCount numBits
      return $ Operator version typeId packets

parseBitsPacketBitCount :: Int -> Emitter Char [BitsPacket]
parseBitsPacketBitCount 0 = pure []
parseBitsPacketBitCount n = do
  beforeCount <- getCounter
  packet <- parseBitsPacket
  afterCount <- getCounter
  let emitCount = afterCount - beforeCount
  let bitsLeft = n - emitCount
  if bitsLeft < 0
    then error $ "Emitted past the end of a packet! [packet = " ++ (show packet) ++ ", count = " ++ (show emitCount) ++ "]"
    else do
      nextPackets <- parseBitsPacketBitCount (n - emitCount)
      return (packet : nextPackets)

parseBitsPackets :: Int -> Emitter Char [BitsPacket]
parseBitsPackets 0 = pure []
parseBitsPackets n = do
  packet <- parseBitsPacket
  nextPackets <- parseBitsPackets (pred n)
  return $ (packet : nextPackets)

parseBitsPacket :: Emitter Char BitsPacket
parseBitsPacket = do
  version <- fromBinary <$> emit 3
  typeId <- fromBinary <$> emit 3
  case typeId of
    4 -> parseLiteral version
    _ -> parseOperator version typeId

parsePacketBinary :: BinaryString -> BitsPacket
parsePacketBinary (BinaryString bs) = evalEmitter parseBitsPacket bs

parsePacket :: HexString -> BitsPacket
parsePacket = parsePacketBinary . toBinary

--
-- Evaluation
--

evaluatePacket :: BitsPacket -> Int
-- Literals: return the value.
evaluatePacket (Literal _ n) = n
-- Type 0: Sum.
evaluatePacket (Operator _ 0 (p:[])) = evaluatePacket p
evaluatePacket (Operator _ 0 ps) = sum $ evaluatePacket <$> ps
-- Type 1: Product.
evaluatePacket (Operator _ 1 (p:[])) = evaluatePacket p
evaluatePacket (Operator _ 1 ps) = product $ evaluatePacket <$> ps
-- Type 2: Minimum.
evaluatePacket (Operator _ 2 ps) = minimum $ evaluatePacket <$> ps
-- Type 3: Maximum.
evaluatePacket (Operator _ 3 ps) = maximum $ evaluatePacket <$> ps
-- Type 5: Greater-than.
evaluatePacket (Operator _ 5 (ap:bp:[])) =
  let a = evaluatePacket ap
      b = evaluatePacket bp
   in if a > b then 1 else 0
evaluatePacket (Operator _ 5 ps) = error $ "Too many packets for greater-than!: " ++ (show ps)
-- Type 6: Less-than.
evaluatePacket (Operator _ 6 (ap:bp:[])) =
  let a = evaluatePacket ap
      b = evaluatePacket bp
   in if a < b then 1 else 0
evaluatePacket (Operator _ 6 ps) = error $ "Too many packets for less-than!: " ++ (show ps)
-- Type 7: Equal-to.
evaluatePacket (Operator _ 7 (ap:bp:[])) =
  let a = evaluatePacket ap
      b = evaluatePacket bp
   in if a == b then 1 else 0
evaluatePacket (Operator _ 7 ps) = error $ "Too many packets for equal-to!: " ++ (show ps)
evaluatePacket (Operator _ v _) = error $ "Invalid operator type!: " ++ (show v)
