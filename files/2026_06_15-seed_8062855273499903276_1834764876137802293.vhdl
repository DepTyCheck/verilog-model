-- Seed: 8062855273499903276,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity fsenttkv is
  port (rdslgfid : in severity_level; zcbjqikcqq : inout integer; eul : buffer std_logic_vector(1 to 0); xhyheue : in character);
end fsenttkv;

architecture ybidezxrtx of fsenttkv is
  
begin
  -- Multi-driven assignments
  eul <= "";
  eul <= (others => '0');
end ybidezxrtx;

library ieee;
use ieee.std_logic_1164.all;

entity fewuad is
  port (qu : inout std_logic; ue : inout integer);
end fewuad;

library ieee;
use ieee.std_logic_1164.all;

architecture ljmuxcno of fewuad is
  signal e : std_logic_vector(1 to 0);
  signal ql : character;
  signal rp : integer;
  signal gi : character;
  signal mimpbi : std_logic_vector(1 to 0);
  signal zkzezhh : integer;
  signal gymhxs : severity_level;
begin
  a : entity work.fsenttkv
    port map (rdslgfid => gymhxs, zcbjqikcqq => zkzezhh, eul => mimpbi, xhyheue => gi);
  mchtxtp : entity work.fsenttkv
    port map (rdslgfid => gymhxs, zcbjqikcqq => rp, eul => mimpbi, xhyheue => ql);
  xajf : entity work.fsenttkv
    port map (rdslgfid => gymhxs, zcbjqikcqq => ue, eul => e, xhyheue => gi);
  
  -- Single-driven assignments
  ql <= 'c';
  gi <= 'd';
  gymhxs <= WARNING;
  
  -- Multi-driven assignments
  qu <= 'L';
  qu <= '1';
end ljmuxcno;



-- Seed after: 5468363054809346739,1834764876137802293
