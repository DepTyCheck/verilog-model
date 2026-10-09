-- Seed: 4122592234168140591,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity jhsduhc is
  port (nvjbqmem : buffer std_logic_vector(1 to 2); zapvlhdo : in real);
end jhsduhc;

architecture ab of jhsduhc is
  
begin
  -- Multi-driven assignments
  nvjbqmem <= nvjbqmem;
end ab;

library ieee;
use ieee.std_logic_1164.all;

entity elqryhwy is
  port (ubejb : buffer std_logic; okowt : out real; dg : out real; dvdo : in time);
end elqryhwy;

library ieee;
use ieee.std_logic_1164.all;

architecture vgwbe of elqryhwy is
  signal qskzvtfuq : real;
  signal p : real;
  signal dbvy : std_logic_vector(1 to 2);
  signal fi : real;
  signal djfiolue : std_logic_vector(1 to 2);
begin
  axqd : entity work.jhsduhc
    port map (nvjbqmem => djfiolue, zapvlhdo => fi);
  m : entity work.jhsduhc
    port map (nvjbqmem => dbvy, zapvlhdo => p);
  vrnfezv : entity work.jhsduhc
    port map (nvjbqmem => djfiolue, zapvlhdo => dg);
  eiadhtf : entity work.jhsduhc
    port map (nvjbqmem => djfiolue, zapvlhdo => qskzvtfuq);
  
  -- Single-driven assignments
  okowt <= dg;
  p <= 3_4_0.01021;
  dg <= 16#B1.239F1#;
  
  -- Multi-driven assignments
  dbvy <= djfiolue;
  ubejb <= ubejb;
end vgwbe;

library ieee;
use ieee.std_logic_1164.all;

entity xmtozi is
  port (ctnopopqc : in time; v : in std_logic);
end xmtozi;

library ieee;
use ieee.std_logic_1164.all;

architecture sbbi of xmtozi is
  signal cxbs : time;
  signal oynbooatr : real;
  signal kvzdito : real;
  signal cuqby : std_logic;
  signal zpbetds : time;
  signal vzhyfh : real;
  signal q : real;
  signal zuhkij : std_logic;
begin
  r : entity work.elqryhwy
    port map (ubejb => zuhkij, okowt => q, dg => vzhyfh, dvdo => zpbetds);
  vqehz : entity work.elqryhwy
    port map (ubejb => cuqby, okowt => kvzdito, dg => oynbooatr, dvdo => cxbs);
  
  -- Single-driven assignments
  zpbetds <= zpbetds;
  cxbs <= 1.0_2_3_2 ns;
  
  -- Multi-driven assignments
  zuhkij <= 'X';
  cuqby <= 'X';
end sbbi;



-- Seed after: 7395578845031297209,8891552411914730853
