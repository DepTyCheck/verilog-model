module Test.Verilog.Defaults

import public Test.Verilog.Connections
import public Test.Verilog.PrintableModules

public export
StdModules : ModuleSigsList
StdModules =
  [ MkModuleSig [Var $ AVar Logic', Var $ AVar Logic'] [Var $ AVar Logic']
  , MkModuleSig [Var $ AVar Logic', Var $ AVar Logic'] [Var $ AVar Logic']
  , MkModuleSig [Var $ AVar Logic', Var $ AVar Logic'] [Var $ AVar Logic']
  , MkModuleSig [Var $ AVar Logic', Var $ AVar Logic'] [Var $ AVar Logic']
  , MkModuleSig [Var $ AVar Logic']                    [Var $ AVar Logic']
  ]

public export
StdModulesPV : PrintableModules StdModules
StdModulesPV =
  [
    MkPrintableModule "and"  (StdModule 2 1)
  , MkPrintableModule "or"   (StdModule 2 1)
  , MkPrintableModule "nand" (StdModule 2 1)
  , MkPrintableModule "xor"  (StdModule 2 1)
  , MkPrintableModule "not"  (StdModule 1 1)
  ]
