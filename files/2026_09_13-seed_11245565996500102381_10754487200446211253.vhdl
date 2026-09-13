-- Seed: 11245565996500102381,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity gzafrwy is
  port (dbwk : inout std_logic; heatm : in std_logic_vector(4 to 2));
end gzafrwy;

architecture msklnsgx of gzafrwy is
  
begin
  -- Multi-driven assignments
  dbwk <= 'L';
end msklnsgx;

entity xtjymiki is
  port (pxadkghujr : linkage bit);
end xtjymiki;

library ieee;
use ieee.std_logic_1164.all;

architecture yilmk of xtjymiki is
  signal dbcnas : std_logic;
  signal fhkjbjf : std_logic_vector(4 to 2);
  signal vfafq : std_logic_vector(4 to 2);
  signal w : std_logic;
begin
  j : entity work.gzafrwy
    port map (dbwk => w, heatm => vfafq);
  nogtx : entity work.gzafrwy
    port map (dbwk => w, heatm => fhkjbjf);
  ovvwyejtk : entity work.gzafrwy
    port map (dbwk => dbcnas, heatm => fhkjbjf);
  orvz : entity work.gzafrwy
    port map (dbwk => w, heatm => vfafq);
  
  -- Multi-driven assignments
  dbcnas <= w;
  w <= '1';
  w <= 'X';
  w <= w;
end yilmk;



-- Seed after: 13607029527575366851,10754487200446211253
