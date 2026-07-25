-- Seed: 5320302142602785922,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (lsmb : inout real_vector(0 to 4); hibpmr : linkage std_logic);
end i;

architecture pobyytb of i is
  
begin
  -- Single-driven assignments
  lsmb <= lsmb;
end pobyytb;

library ieee;
use ieee.std_logic_1164.all;

entity rkcnm is
  port (r : buffer std_logic; bwl : in integer; xmefwvf : in time);
end rkcnm;

library ieee;
use ieee.std_logic_1164.all;

architecture didibo of rkcnm is
  signal swdtkjkbys : std_logic;
  signal jrg : real_vector(0 to 4);
  signal dbmyvtqu : std_logic;
  signal hyvfg : real_vector(0 to 4);
begin
  tzbymyaaxi : entity work.i
    port map (lsmb => hyvfg, hibpmr => dbmyvtqu);
  mbuwoys : entity work.i
    port map (lsmb => jrg, hibpmr => swdtkjkbys);
  
  -- Multi-driven assignments
  dbmyvtqu <= r;
  r <= 'Z';
  r <= 'U';
end didibo;



-- Seed after: 11440606403542764038,5306691039457971049
