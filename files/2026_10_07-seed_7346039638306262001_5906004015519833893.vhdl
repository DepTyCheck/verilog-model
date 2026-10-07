-- Seed: 7346039638306262001,5906004015519833893

entity pzgr is
  port (ziin : buffer time);
end pzgr;

architecture tp of pzgr is
  
begin
  -- Single-driven assignments
  ziin <= 130.0 fs;
end tp;

library ieee;
use ieee.std_logic_1164.all;

entity slel is
  port (gao : in real; djynjw : inout std_logic_vector(4 to 3));
end slel;

architecture lossko of slel is
  signal ie : time;
  signal snch : time;
begin
  gxu : entity work.pzgr
    port map (ziin => snch);
  ikccrfxsi : entity work.pzgr
    port map (ziin => ie);
  
  -- Multi-driven assignments
  djynjw <= djynjw;
  djynjw <= "";
end lossko;

library ieee;
use ieee.std_logic_1164.all;

entity oyhimob is
  port (merj : buffer std_logic);
end oyhimob;

library ieee;
use ieee.std_logic_1164.all;

architecture ahucf of oyhimob is
  signal rjhasgd : time;
  signal hithwrd : time;
  signal orfjfqleje : time;
  signal niqaveesvm : std_logic_vector(4 to 3);
  signal omxs : real;
begin
  auibyxewv : entity work.slel
    port map (gao => omxs, djynjw => niqaveesvm);
  fjbc : entity work.pzgr
    port map (ziin => orfjfqleje);
  ddzgyi : entity work.pzgr
    port map (ziin => hithwrd);
  ogqeafix : entity work.pzgr
    port map (ziin => rjhasgd);
  
  -- Single-driven assignments
  omxs <= omxs;
  
  -- Multi-driven assignments
  merj <= 'X';
  merj <= 'X';
  niqaveesvm <= niqaveesvm;
end ahucf;



-- Seed after: 6358453152208879960,5906004015519833893
