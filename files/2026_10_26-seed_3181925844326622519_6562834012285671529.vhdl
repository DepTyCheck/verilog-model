-- Seed: 3181925844326622519,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity lmpnsyuneh is
  port (hjexeyoikm : in time; vsuhmyt : out std_logic_vector(3 to 2));
end lmpnsyuneh;

architecture v of lmpnsyuneh is
  
begin
  -- Multi-driven assignments
  vsuhmyt <= "";
end v;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (dulwievg : inout real_vector(4 downto 3); uz : inout std_logic_vector(2 downto 0));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture comzi of n is
  signal iywhvijywa : time;
  signal nnjtna : std_logic_vector(3 to 2);
  signal jm : time;
  signal xmxgodwio : std_logic_vector(3 to 2);
  signal vff : time;
begin
  syo : entity work.lmpnsyuneh
    port map (hjexeyoikm => vff, vsuhmyt => xmxgodwio);
  rbdoq : entity work.lmpnsyuneh
    port map (hjexeyoikm => jm, vsuhmyt => nnjtna);
  redh : entity work.lmpnsyuneh
    port map (hjexeyoikm => iywhvijywa, vsuhmyt => xmxgodwio);
  
  -- Single-driven assignments
  jm <= vff;
  vff <= jm;
  dulwievg <= (2_1_0.1_0, 11201.0);
  
  -- Multi-driven assignments
  uz <= uz;
  uz <= "L1X";
  xmxgodwio <= "";
  nnjtna <= "";
end comzi;

library ieee;
use ieee.std_logic_1164.all;

entity szbb is
  port (k : in std_logic_vector(1 to 0));
end szbb;

library ieee;
use ieee.std_logic_1164.all;

architecture xwgy of szbb is
  signal zsvutcen : std_logic_vector(3 to 2);
  signal svcp : std_logic_vector(3 to 2);
  signal xscudtrllc : time;
begin
  l : entity work.lmpnsyuneh
    port map (hjexeyoikm => xscudtrllc, vsuhmyt => svcp);
  gromnux : entity work.lmpnsyuneh
    port map (hjexeyoikm => xscudtrllc, vsuhmyt => zsvutcen);
  tqnfmpnk : entity work.lmpnsyuneh
    port map (hjexeyoikm => xscudtrllc, vsuhmyt => svcp);
  
  -- Multi-driven assignments
  svcp <= "";
  zsvutcen <= (others => '0');
  svcp <= zsvutcen;
  svcp <= k;
end xwgy;



-- Seed after: 13376317072327787487,6562834012285671529
