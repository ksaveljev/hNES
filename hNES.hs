import qualified Data.ByteString as B

import NES.Emulator
import NES.Emulator.TestEmulator

test :: B.ByteString -> ()
test program = runTestEmulator $ do
  loadProgram program
  emulateCycles 100

main :: IO ()
main = do
    program <- B.readFile "nestest.nes"
    let x = test program
    seq x $ return ()
