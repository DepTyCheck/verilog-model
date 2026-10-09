-- Seed: 15802013204706221079,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity vhx is
  port (hvlrycstn : in std_logic_vector(2 downto 0); q : inout severity_level);
end vhx;

architecture ar of vhx is
  
begin
  -- Single-driven assignments
  q <= ERROR;
end ar;

library ieee;
use ieee.std_logic_1164.all;

entity spk is
  port (bhhwnsr : buffer integer; rjbycvn : buffer std_logic);
end spk;

library ieee;
use ieee.std_logic_1164.all;

architecture rx of spk is
  signal tyi : severity_level;
  signal jtmcjymbt : std_logic_vector(2 downto 0);
begin
  njeaeh : entity work.vhx
    port map (hvlrycstn => jtmcjymbt, q => tyi);
  
  -- Multi-driven assignments
  rjbycvn <= 'U';
end rx;



-- Seed after: 300004933860902300,8891552411914730853
