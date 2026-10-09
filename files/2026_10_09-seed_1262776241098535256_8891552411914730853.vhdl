-- Seed: 1262776241098535256,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (nmhj : inout std_logic; v : in time);
end t;

architecture zmmpz of t is
  
begin
  -- Multi-driven assignments
  nmhj <= 'X';
  nmhj <= '-';
end zmmpz;

library ieee;
use ieee.std_logic_1164.all;

entity jzi is
  port (maksu : in integer; shs : out severity_level; gfwrydrtaa : buffer time; llmeis : in std_logic);
end jzi;

library ieee;
use ieee.std_logic_1164.all;

architecture easgqjh of jzi is
  signal tafxyvb : time;
  signal nggr : std_logic;
begin
  ebbdrk : entity work.t
    port map (nmhj => nggr, v => tafxyvb);
  yrv : entity work.t
    port map (nmhj => nggr, v => gfwrydrtaa);
  
  -- Single-driven assignments
  gfwrydrtaa <= 16#B_F_2_C_E# us;
  tafxyvb <= 3 sec;
  shs <= NOTE;
  
  -- Multi-driven assignments
  nggr <= 'L';
  nggr <= 'L';
  nggr <= 'H';
  nggr <= llmeis;
end easgqjh;



-- Seed after: 729890142059873859,8891552411914730853
