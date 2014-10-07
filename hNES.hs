import qualified Data.ByteString as B

import NES.ROM
import NES.Emulator (loadProgram)
import NES.Emulator.Debug
import NES.Emulator.TestEmulator

test :: B.ByteString -> ()
test program = runTestEmulator (loadROM program) $ do
  loadProgram program
  emulateCycles 8000

main :: IO ()
main = do
    program <- B.readFile "nestest.nes"
    let x = test program
    seq x $ return ()
