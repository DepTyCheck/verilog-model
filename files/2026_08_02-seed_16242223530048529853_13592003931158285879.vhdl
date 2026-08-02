-- Seed: 16242223530048529853,13592003931158285879

entity zjqe is
  port (sc : linkage integer; sybj : inout boolean_vector(2 downto 4));
end zjqe;

architecture ri of zjqe is
  
begin
  -- Single-driven assignments
  sybj <= (others => TRUE);
end ri;

entity b is
  port (iouw : linkage real; wtfrin : in time; sb : linkage time_vector(1 to 4));
end b;

architecture lfipjq of b is
  signal o : boolean_vector(2 downto 4);
  signal hkurz : integer;
  signal qphptsxp : boolean_vector(2 downto 4);
  signal vg : integer;
begin
  r : entity work.zjqe
    port map (sc => vg, sybj => qphptsxp);
  e : entity work.zjqe
    port map (sc => hkurz, sybj => o);
end lfipjq;

library ieee;
use ieee.std_logic_1164.all;

entity rbitzb is
  port (bbvyudj : inout bit; p : inout severity_level; kjblje : buffer std_logic_vector(3 to 3));
end rbitzb;

architecture eptvildfnz of rbitzb is
  signal oemmpbam : boolean_vector(2 downto 4);
  signal na : integer;
  signal wv : time_vector(1 to 4);
  signal vyrgka : time;
  signal nhj : real;
  signal qwbwyhw : boolean_vector(2 downto 4);
  signal owclsksmh : integer;
begin
  fwgbqrtkx : entity work.zjqe
    port map (sc => owclsksmh, sybj => qwbwyhw);
  wjqm : entity work.b
    port map (iouw => nhj, wtfrin => vyrgka, sb => wv);
  qqovsbhfqu : entity work.zjqe
    port map (sc => na, sybj => oemmpbam);
  
  -- Single-driven assignments
  p <= NOTE;
  vyrgka <= 1 sec;
  bbvyudj <= bbvyudj;
end eptvildfnz;

entity oxaxt is
  port (gqhrf : inout real);
end oxaxt;

architecture m of oxaxt is
  
begin
  
end m;



-- Seed after: 13447090110327232923,13592003931158285879
