-- Seed: 5984185403564817460,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity bgxzohjtme is
  port (pmhv : in time; egir : linkage time; knobg : buffer std_logic_vector(3 downto 1));
end bgxzohjtme;

architecture ls of bgxzohjtme is
  
begin
  
end ls;

library ieee;
use ieee.std_logic_1164.all;

entity ejxfme is
  port (uhqjfs : linkage std_logic; ukspltd : inout time);
end ejxfme;

library ieee;
use ieee.std_logic_1164.all;

architecture d of ejxfme is
  signal ikutakrw : std_logic_vector(3 downto 1);
  signal t : time;
  signal mk : std_logic_vector(3 downto 1);
begin
  vj : entity work.bgxzohjtme
    port map (pmhv => ukspltd, egir => ukspltd, knobg => mk);
  zwzivriac : entity work.bgxzohjtme
    port map (pmhv => t, egir => t, knobg => ikutakrw);
  
  -- Multi-driven assignments
  ikutakrw <= ('L', '-', 'U');
  mk <= "00H";
end d;

library ieee;
use ieee.std_logic_1164.all;

entity dmdijccx is
  port (dmeo : in std_logic; rrjmzfox : buffer time; rbfj : in std_logic_vector(1 downto 0); p : linkage character);
end dmdijccx;

library ieee;
use ieee.std_logic_1164.all;

architecture bodiqjqtm of dmdijccx is
  signal kz : time;
  signal bd : std_logic;
begin
  lffodxrf : entity work.ejxfme
    port map (uhqjfs => bd, ukspltd => kz);
  
  -- Single-driven assignments
  rrjmzfox <= 1324 us;
  
  -- Multi-driven assignments
  bd <= 'X';
  bd <= 'X';
  bd <= 'W';
end bodiqjqtm;



-- Seed after: 7139923124984724226,6882842853887419669
