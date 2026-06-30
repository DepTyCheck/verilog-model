-- Seed: 6463474837314925626,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity nbg is
  port (psif : inout std_logic_vector(3 to 0); drgcsyxsbo : in integer_vector(3 downto 1); ffuoxhyy : inout boolean);
end nbg;

architecture vfgxqeavi of nbg is
  
begin
  -- Single-driven assignments
  ffuoxhyy <= TRUE;
  
  -- Multi-driven assignments
  psif <= "";
  psif <= "";
  psif <= "";
  psif <= "";
end vfgxqeavi;

library ieee;
use ieee.std_logic_1164.all;

entity soxqutj is
  port (dmkm : out boolean; tjq : buffer boolean_vector(1 downto 2); y : out std_logic);
end soxqutj;

library ieee;
use ieee.std_logic_1164.all;

architecture u of soxqutj is
  signal zeabezqk : boolean;
  signal szdwyvnirj : std_logic_vector(3 to 0);
  signal sygaxbz : integer_vector(3 downto 1);
  signal b : std_logic_vector(3 to 0);
begin
  yrf : entity work.nbg
    port map (psif => b, drgcsyxsbo => sygaxbz, ffuoxhyy => dmkm);
  vqvonhjht : entity work.nbg
    port map (psif => szdwyvnirj, drgcsyxsbo => sygaxbz, ffuoxhyy => zeabezqk);
end u;



-- Seed after: 16816968361350779109,14629254427735353553
