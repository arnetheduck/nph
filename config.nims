# begin Nimble config (version 2)
when withDir(thisDir(), system.fileExists("nimble.paths")):
  include "nimble.paths"
# end Nimble config

switch("p", "$projectDir/nimbledeps/pkgs2/nim-2.0.0-35b9d926d314ce8c3326902c138f5c2247cfd390/compiler")
switch("p", "$projectDir/../nimbledeps/pkgs2/nim-2.0.0-35b9d926d314ce8c3326902c138f5c2247cfd390/compiler")
