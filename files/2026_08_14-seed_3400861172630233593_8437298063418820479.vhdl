-- Seed: 3400861172630233593,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity fxshcrgbxz is
  port (gen : inout std_logic_vector(0 to 0));
end fxshcrgbxz;

architecture ebgs of fxshcrgbxz is
  
begin
  -- Multi-driven assignments
  gen <= "U";
end ebgs;

entity rdaaaafvz is
  port (sik : out real; y : linkage severity_level);
end rdaaaafvz;

library ieee;
use ieee.std_logic_1164.all;

architecture tmib of rdaaaafvz is
  signal x : std_logic_vector(0 to 0);
begin
  i : entity work.fxshcrgbxz
    port map (gen => x);
  ceck : entity work.fxshcrgbxz
    port map (gen => x);
  snrbwxkm : entity work.fxshcrgbxz
    port map (gen => x);
  
  -- Single-driven assignments
  sik <= sik;
  
  -- Multi-driven assignments
  x <= "1";
  x <= x;
  x <= "L";
end tmib;



-- Seed after: 7193999964719609137,8437298063418820479
