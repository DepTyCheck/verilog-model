-- Seed: 13470383343689807987,3687118713772291287

entity utstkvhom is
  port (pea : buffer integer; tbs : in integer);
end utstkvhom;

architecture mct of utstkvhom is
  
begin
  -- Single-driven assignments
  pea <= 2#0_1#;
end mct;

library ieee;
use ieee.std_logic_1164.all;

entity jh is
  port (zjfnoncf : out real; pqs : linkage real; bbfb : out std_logic);
end jh;

architecture vdxgradc of jh is
  signal mwb : integer;
  signal jobmxy : integer;
  signal ehyvkn : integer;
  signal zc : integer;
begin
  ordt : entity work.utstkvhom
    port map (pea => zc, tbs => ehyvkn);
  lh : entity work.utstkvhom
    port map (pea => ehyvkn, tbs => jobmxy);
  kyxeqypg : entity work.utstkvhom
    port map (pea => mwb, tbs => zc);
  
  -- Multi-driven assignments
  bbfb <= 'H';
end vdxgradc;

entity elxltedhw is
  port (ejby : inout real);
end elxltedhw;

library ieee;
use ieee.std_logic_1164.all;

architecture ktk of elxltedhw is
  signal hsbzds : integer;
  signal gamip : std_logic;
  signal q : real;
  signal mmgftziwto : real;
begin
  rewvfxncs : entity work.jh
    port map (zjfnoncf => mmgftziwto, pqs => q, bbfb => gamip);
  jejladqna : entity work.utstkvhom
    port map (pea => hsbzds, tbs => hsbzds);
  
  -- Single-driven assignments
  ejby <= 16#0.1D7#;
  
  -- Multi-driven assignments
  gamip <= 'U';
end ktk;



-- Seed after: 2966663108790601794,3687118713772291287
