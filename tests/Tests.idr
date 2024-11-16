module Tests

import Test.Golden.RunnerHelper

main : IO ()
main = goldenRunner $
  [ "Printer" `atDir` "printer" ]
