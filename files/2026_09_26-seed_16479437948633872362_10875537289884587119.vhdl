-- Seed: 16479437948633872362,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity patvy is
  port (dyfktbi : in bit_vector(2 downto 3); saymgwo : inout std_logic);
end patvy;

architecture siuvuzfhgs of patvy is
  
begin
  -- Multi-driven assignments
  saymgwo <= 'H';
end siuvuzfhgs;

library ieee;
use ieee.std_logic_1164.all;

entity rqoadcurf is
  port (iqki : linkage std_logic);
end rqoadcurf;

library ieee;
use ieee.std_logic_1164.all;

architecture ztxu of rqoadcurf is
  signal byyagrdd : std_logic;
  signal wsom : bit_vector(2 downto 3);
  signal tyqlwpgmr : std_logic;
  signal eyujnf : std_logic;
  signal gm : bit_vector(2 downto 3);
begin
  njjwbx : entity work.patvy
    port map (dyfktbi => gm, saymgwo => eyujnf);
  xmcjlwkftd : entity work.patvy
    port map (dyfktbi => gm, saymgwo => tyqlwpgmr);
  zeqf : entity work.patvy
    port map (dyfktbi => wsom, saymgwo => eyujnf);
  bda : entity work.patvy
    port map (dyfktbi => gm, saymgwo => byyagrdd);
  
  -- Single-driven assignments
  wsom <= (others => '0');
  gm <= gm;
  
  -- Multi-driven assignments
  eyujnf <= 'X';
  byyagrdd <= '-';
  tyqlwpgmr <= 'H';
  byyagrdd <= eyujnf;
end ztxu;



-- Seed after: 2384148740588122036,10875537289884587119
