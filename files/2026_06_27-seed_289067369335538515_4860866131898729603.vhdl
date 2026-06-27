-- Seed: 289067369335538515,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity lvlfxf is
  port (fengurz : buffer std_logic);
end lvlfxf;

architecture hwpb of lvlfxf is
  
begin
  -- Multi-driven assignments
  fengurz <= 'L';
  fengurz <= 'X';
  fengurz <= 'W';
  fengurz <= 'U';
end hwpb;

entity mqrxu is
  port (aafkukvi : inout time);
end mqrxu;

library ieee;
use ieee.std_logic_1164.all;

architecture bepd of mqrxu is
  signal pxrugziony : std_logic;
  signal igmvv : std_logic;
begin
  sojwlbffc : entity work.lvlfxf
    port map (fengurz => igmvv);
  gayurlapn : entity work.lvlfxf
    port map (fengurz => pxrugziony);
  
  -- Single-driven assignments
  aafkukvi <= 16#9_7# us;
end bepd;

library ieee;
use ieee.std_logic_1164.all;

entity cjbnd is
  port (lrobt : buffer std_logic; vicsj : linkage character);
end cjbnd;

library ieee;
use ieee.std_logic_1164.all;

architecture eej of cjbnd is
  signal hq : std_logic;
  signal hjasehvvlh : time;
  signal zgn : time;
begin
  nnpddhbkc : entity work.mqrxu
    port map (aafkukvi => zgn);
  tnkx : entity work.mqrxu
    port map (aafkukvi => hjasehvvlh);
  vvcxdk : entity work.lvlfxf
    port map (fengurz => hq);
  
  -- Multi-driven assignments
  lrobt <= 'X';
  lrobt <= 'W';
  hq <= 'X';
end eej;

library ieee;
use ieee.std_logic_1164.all;

entity nxruzxeaz is
  port (iqr : linkage std_logic_vector(3 to 4); bv : out integer; aivbsbbm : out time; rzohs : inout real);
end nxruzxeaz;

architecture tk of nxruzxeaz is
  
begin
  -- Single-driven assignments
  aivbsbbm <= 16#0# fs;
  rzohs <= 2#1.1_0_1_1_1#;
  bv <= 16#0#;
end tk;



-- Seed after: 16807291137475202564,4860866131898729603
