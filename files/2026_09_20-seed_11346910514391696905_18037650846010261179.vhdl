-- Seed: 11346910514391696905,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (enaxtr : buffer real; isldtduw : in std_logic_vector(1 downto 1));
end o;

architecture xh of o is
  
begin
  -- Single-driven assignments
  enaxtr <= 3.3_2_3_3;
end xh;

library ieee;
use ieee.std_logic_1164.all;

entity okrnchxrh is
  port (mhiibxzt : in std_logic_vector(3 to 2); x : out bit);
end okrnchxrh;

library ieee;
use ieee.std_logic_1164.all;

architecture uvwydt of okrnchxrh is
  signal eg : real;
  signal odxgmn : std_logic_vector(1 downto 1);
  signal dcuojifpo : real;
begin
  hbqwtdu : entity work.o
    port map (enaxtr => dcuojifpo, isldtduw => odxgmn);
  tmpbfcr : entity work.o
    port map (enaxtr => eg, isldtduw => odxgmn);
  
  -- Single-driven assignments
  x <= x;
  
  -- Multi-driven assignments
  odxgmn <= "W";
  odxgmn <= "W";
  odxgmn <= "H";
  odxgmn <= odxgmn;
end uvwydt;



-- Seed after: 4637337966737706302,18037650846010261179
