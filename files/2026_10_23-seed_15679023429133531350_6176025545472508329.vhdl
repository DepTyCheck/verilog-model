-- Seed: 15679023429133531350,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity xbm is
  port (ou : linkage std_logic_vector(1 to 3); bofbfvbg : out severity_level; n : buffer std_logic);
end xbm;

architecture lwckpqxqrh of xbm is
  
begin
  -- Single-driven assignments
  bofbfvbg <= WARNING;
end lwckpqxqrh;

library ieee;
use ieee.std_logic_1164.all;

entity yibdqc is
  port (dtr : inout std_logic_vector(0 to 0); hxkk : buffer severity_level);
end yibdqc;

library ieee;
use ieee.std_logic_1164.all;

architecture unnk of yibdqc is
  signal eoexmkjw : std_logic;
  signal owbooycuw : std_logic;
  signal efqfv : severity_level;
  signal pqox : std_logic_vector(1 to 3);
begin
  vn : entity work.xbm
    port map (ou => pqox, bofbfvbg => efqfv, n => owbooycuw);
  wlfqqx : entity work.xbm
    port map (ou => pqox, bofbfvbg => hxkk, n => eoexmkjw);
  
  -- Multi-driven assignments
  dtr <= (others => '1');
end unnk;



-- Seed after: 14044528696934640697,6176025545472508329
