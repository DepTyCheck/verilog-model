module Test.VHDL.Pretty

import Data.Fuel

import Test.VHDL.VHDLDesign

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy

export
prettyDesign : {opts : _} -> Fuel -> VHDLDesign -> Gen0 $ Doc opts
prettyDesign x _ = pure $ vsep
  [
    line "entity helloworld is"
  , line "end helloworld;"
  , line ""
  , line "architecture behaviour of helloworld is"
  , line "begin"
  , line "end behaviour;"
 ]
