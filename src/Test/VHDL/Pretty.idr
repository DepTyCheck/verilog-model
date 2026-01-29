module Test.VHDL.Pretty

import Data.Fuel

import Test.Common.Design
import public Test.Common.PrintableDesigns
import public Test.Common.UniqueNames
import Test.VHDL.VHDLDesign
import public Test.VHDL.UniqueNames

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy

parameters {opts : LayoutOpts} (entityName : String)

  printEntity : Gen0 $ Doc opts
  printEntity = do
    pure $ vsep [
      line "entity \{entityName} is"
    , line "end \{entityName};"
    ]

parameters {opts : LayoutOpts} (entityName : String)

  printArchitecture : {opts : _} -> Gen0 $ Doc opts
  printArchitecture = do
    pure $ vsep [
      line "architecture behaviour of \{entityName} is"
    , line "begin"
    , line "end behaviour;"
    ]

export
prettyDesign : {opts : _} -> {dus : _} -> Fuel -> 
               (pds : PrintableDesigns VHDL dus) -> UniqNames dus.length (allDesignNames pds) => VHDLDesign dus -> Gen0 $ Doc opts
prettyDesign _ _         End                    = pure $ empty
prettyDesign x pds @{un} (New u {uu} subUs mcs cont) = do
  (entityName ** connNames ** subEntitiesNames ** unEntConnSub) <- genPDNames VHDLKeywords x pds {un} {uu} subUs mcs

  -- let generatedPrintableInfo : ?
  --     generatedPrintableInfo = MkPrintableDesign entityName (UserModule inputNames outputNames)
  -- Recursive call to use at the end
  -- recur <- prettyDesign x ?a ?b --(?generatedPrintableInfo :: pds) cont

  ent <- printEntity {opts} entityName
  arch <- printArchitecture {opts} entityName
  pure $ vsep
    [
      ent,
      line "",
      arch
  ]
