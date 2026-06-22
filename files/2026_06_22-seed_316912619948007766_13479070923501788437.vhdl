-- Seed: 316912619948007766,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity jsunfcxfxf is
  port (kwxvdmn : buffer time_vector(1 to 4); hrtizej : linkage std_logic);
end jsunfcxfxf;

architecture gpsa of jsunfcxfxf is
  
begin
  -- Single-driven assignments
  kwxvdmn <= (0 hr, 16#F.6F7FB# us, 4_2_1_2_3.13 ms, 3 min);
end gpsa;

library ieee;
use ieee.std_logic_1164.all;

entity gyhp is
  port (ymsjf : buffer real; ugciqbx : in std_logic; nxqaw : out time);
end gyhp;

library ieee;
use ieee.std_logic_1164.all;

architecture h of gyhp is
  signal hlukozwuhq : time_vector(1 to 4);
  signal tseeqsmmj : time_vector(1 to 4);
  signal y : std_logic;
  signal yosbalbqj : time_vector(1 to 4);
begin
  t : entity work.jsunfcxfxf
    port map (kwxvdmn => yosbalbqj, hrtizej => y);
  hpbgjz : entity work.jsunfcxfxf
    port map (kwxvdmn => tseeqsmmj, hrtizej => ugciqbx);
  vpddqc : entity work.jsunfcxfxf
    port map (kwxvdmn => hlukozwuhq, hrtizej => ugciqbx);
  
  -- Single-driven assignments
  nxqaw <= 3 min;
  ymsjf <= 2#0_0_0.1_0#;
  
  -- Multi-driven assignments
  y <= 'W';
  y <= 'H';
  y <= 'X';
end h;



-- Seed after: 15763410868186616628,13479070923501788437
