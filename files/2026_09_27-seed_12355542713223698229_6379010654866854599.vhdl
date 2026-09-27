-- Seed: 12355542713223698229,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity akb is
  port (mofzii : linkage real_vector(0 to 3); hre : inout std_logic_vector(4 downto 3));
end akb;

architecture o of akb is
  
begin
  -- Multi-driven assignments
  hre <= ('Z', 'X');
  hre <= hre;
  hre <= "1H";
end o;

library ieee;
use ieee.std_logic_1164.all;

entity gyder is
  port (ziaiu : in time; ykepasoio : inout std_logic_vector(0 downto 4));
end gyder;

library ieee;
use ieee.std_logic_1164.all;

architecture yv of gyder is
  signal vz : real_vector(0 to 3);
  signal albsy : std_logic_vector(4 downto 3);
  signal kzxovylwfq : real_vector(0 to 3);
begin
  bg : entity work.akb
    port map (mofzii => kzxovylwfq, hre => albsy);
  ltzsrv : entity work.akb
    port map (mofzii => vz, hre => albsy);
  
  -- Multi-driven assignments
  ykepasoio <= ykepasoio;
  albsy <= "UL";
end yv;



-- Seed after: 18135630810657080961,6379010654866854599
