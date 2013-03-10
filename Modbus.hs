module Modbus
  ( ModRequest(..)
  , ModResponse(..)
  , ModRequestFrame(..)
  , ModResponseFrame(..)
  ) where

import Control.Monad
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize
import Data.Word
import Data.Digest.CRC16

type ModAddress = Word16
type StationAddress = Word8
type FunctionCode = Word8

data ModRequestFrame = ModRequestFrame StationAddress ModRequest deriving (Show)
data ModResponseFrame = ModResponseFrame StationAddress ModResponse deriving (Show)

instance Serialize ModRequestFrame where
    get = getFrame ModRequestFrame
    put (ModRequestFrame id req) = putFrame id req

instance Serialize ModResponseFrame where
    get = getFrame ModResponseFrame
    put (ModResponseFrame id req) = putFrame id req

putFrame id req = 
    putWord8 id >> putByteString body >> putWord16le (crc16 packet)
  where
    body = encode req
    packet = B.unpack $ B.cons id body

getFrame cons = do
    id <- get
    req <- get
    crc <- getWord16le
    when (crc /= crc' id req) $ fail "CRC check failed"
    return $ cons id req
  where
    crc' id req = crc16 . B.unpack . B.cons id $ encode req


data ModRequest 
    = ReadCoils ModAddress Word16
    | ReadDiscreteInputs ModAddress Word16
    | ReadHoldingRegisters ModAddress Word16
    | ReadInputRegisters ModAddress Word16
    | WriteSingleCoil ModAddress Word16
    | WriteSingleRegister ModAddress Word16
    | WriteMultipleCoils ModAddress Word16 Word8 ByteString
    | WriteMultipleRegisters ModAddress Word16 Word8 ByteString
    deriving (Show)

instance Serialize ModRequest where
    get = do 
        fn <- getWord8
        case fn of
            1  -> f ReadCoils
            2  -> f ReadDiscreteInputs
            3  -> f ReadHoldingRegisters
            4  -> f ReadInputRegisters
            5  -> f WriteSingleCoil
            6  -> f WriteSingleRegister
            15 -> f' WriteMultipleCoils
            16 -> f' WriteMultipleRegisters
            _  -> fail $ "Unsupported function code: " ++ show fn
      where
        f cons = cons <$> getWord16be <*> getWord16be
        f' cons = do
            addr  <- getWord16be
            quant <- getWord16be
            count <- getWord8
            body  <- getBytes (fromIntegral count)
            return $ cons addr quant count body
    put req = case req of
        (ReadCoils addr cnt)            -> f 1 addr cnt
        (ReadDiscreteInputs addr cnt)   -> f 2 addr cnt
        (ReadHoldingRegisters addr cnt) -> f 3 addr cnt
        (ReadInputRegisters addr cnt)   -> f 4 addr cnt
        (WriteSingleCoil addr cnt)      -> f 5 addr cnt
        (WriteSingleRegister addr cnt)  -> f 6 addr cnt
        (WriteMultipleCoils addr qnt cnt b)     -> f' 15 addr qnt cnt b
        (WriteMultipleRegisters addr qnt cnt b) -> f' 16 addr qnt cnt b
      where
        f fn addr cnt = putWord8 fn >> putWord16be addr >> putWord16be cnt
        f' fn addr qnt cnt b = putWord8 fn >> putWord16be addr >>
            putWord16be qnt >> putWord8 cnt >> putByteString b 


data ModResponse 
    = ReadCoilsResponse Word8 ByteString
    | ReadDiscreteInputsResponse Word8 ByteString
    | ReadHoldingRegistersResponse Word8 ByteString
    | ReadInputRegistersResponse Word8 ByteString
    | WriteSingleCoilResponse ModAddress Word16
    | WriteSingleRegisterResponse ModAddress Word16
    | WriteMultipleCoilsResponse ModAddress Word16
    | WriteMultipleRegistersResponse ModAddress Word16
    | ExceptionResponse FunctionCode ExceptionCode
    | UnknownFunctionResponse FunctionCode
    deriving (Show)

instance Serialize ModResponse where
    get = do 
        fn <- getWord8
        case fn of
            1  -> f ReadCoilsResponse
            2  -> f ReadDiscreteInputsResponse
            3  -> f ReadHoldingRegistersResponse
            4  -> f ReadInputRegistersResponse
            5  -> f' WriteSingleCoilResponse
            6  -> f' WriteSingleRegisterResponse
            15 -> f' WriteMultipleCoilsResponse
            16 -> f' WriteMultipleRegistersResponse
            x | x >= 0x80 -> ExceptionResponse x <$> get
            _  -> return $ UnknownFunctionResponse fn
      where
        f cons = do
            count <- getWord8
            body  <- getBytes (fromIntegral count)
            return $ cons count body
        f' cons = do
            addr <- getWord16be
            body <- getWord16be
            return $ cons addr body
    put req = case req of
        (ReadCoilsResponse cnt b)            -> f 1 cnt b
        (ReadDiscreteInputsResponse cnt b)   -> f 2 cnt b
        (ReadHoldingRegistersResponse cnt b) -> f 3 cnt b
        (ReadInputRegistersResponse cnt b)   -> f 4 cnt b
        (WriteSingleCoilResponse addr b)      -> f' 5 addr b
        (WriteSingleRegisterResponse addr b)  -> f' 6 addr b
        (WriteMultipleCoilsResponse addr b)     -> f' 15 addr b
        (WriteMultipleRegistersResponse addr b) -> f' 16 addr b
        (ExceptionResponse fn ec)    -> put fn >> put ec
        (UnknownFunctionResponse fn) -> put fn
      where
        f fn cnt b = putWord8 fn >> putWord8 cnt >> putByteString b
        f' fn addr b = putWord8 fn >> putWord16be addr >> putWord16be b


data ExceptionCode 
    = IllegalFunction
    | IllegalDataAddress
    | IllegalDataValue
    | ServerDeviceFailure
    | Acknowledge
    | ServerDeviceBusy
    | MemoryParityError
    | GatewayPathUnavailible
    | GatewayTargetFailedToRespond
    | UnknownExceptionCode Word8
    deriving Show

instance Serialize ExceptionCode where
    put ec = putWord8 $ case ec of
        IllegalDataAddress           -> 0x02
        IllegalDataValue             -> 0x03
        ServerDeviceFailure          -> 0x04
        Acknowledge                  -> 0x05
        ServerDeviceBusy             -> 0x06
        MemoryParityError            -> 0x08
        GatewayPathUnavailible       -> 0x0A
        GatewayTargetFailedToRespond -> 0x0B
        (UnknownExceptionCode x)     -> x
    get = do
        c <- getWord8
        return $ case c of
          0x02 -> IllegalDataAddress
          0x03 -> IllegalDataValue
          0x04 -> ServerDeviceFailure
          0x05 -> Acknowledge
          0x06 -> ServerDeviceBusy
          0x08 -> MemoryParityError
          0x0A -> GatewayPathUnavailible
          0x0B -> GatewayTargetFailedToRespond
          x    -> UnknownExceptionCode x


