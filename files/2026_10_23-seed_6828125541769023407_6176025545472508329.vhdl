-- Seed: 6828125541769023407,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (xqmuanw : buffer std_logic_vector(2 to 3); khw : out time; gvrpnr : buffer time);
end o;

architecture mcanxai of o is
  
begin
  -- Single-driven assignments
  khw <= gvrpnr;
  gvrpnr <= gvrpnr;
  
  -- Multi-driven assignments
  xqmuanw <= xqmuanw;
end mcanxai;

entity e is
  port (ohgr : inout severity_level);
end e;

architecture vafycowmnt of e is
  
begin
  -- Single-driven assignments
  ohgr <= NOTE;
end vafycowmnt;

library ieee;
use ieee.std_logic_1164.all;

entity fow is
  port (d : out std_logic_vector(4 to 2); plkeurya : linkage time);
end fow;

library ieee;
use ieee.std_logic_1164.all;

architecture kyjj of fow is
  signal kscdnyt : time;
  signal ixnju : time;
  signal tquzraldpx : std_logic_vector(2 to 3);
  signal lhxnihk : time;
  signal qujqq : time;
  signal ucoqrgw : std_logic_vector(2 to 3);
begin
  cbqakbfnzn : entity work.o
    port map (xqmuanw => ucoqrgw, khw => qujqq, gvrpnr => lhxnihk);
  w : entity work.o
    port map (xqmuanw => tquzraldpx, khw => ixnju, gvrpnr => kscdnyt);
  
  -- Multi-driven assignments
  d <= "";
  tquzraldpx <= ('L', 'Z');
  d <= "";
end kyjj;

library ieee;
use ieee.std_logic_1164.all;

entity yontvxpuwr is
  port (yuwidcx : in integer; mhp : out std_logic);
end yontvxpuwr;

library ieee;
use ieee.std_logic_1164.all;

architecture yadlmplmli of yontvxpuwr is
  signal rpn : time;
  signal raxvemoqi : time;
  signal ebxyxm : std_logic_vector(2 to 3);
  signal nhztmu : time;
  signal ttijsujzw : time;
  signal fjwpfz : time;
  signal w : std_logic_vector(2 to 3);
  signal r : time;
  signal euqjdh : std_logic_vector(4 to 2);
begin
  rk : entity work.fow
    port map (d => euqjdh, plkeurya => r);
  exz : entity work.o
    port map (xqmuanw => w, khw => fjwpfz, gvrpnr => ttijsujzw);
  vtxdx : entity work.fow
    port map (d => euqjdh, plkeurya => nhztmu);
  qbcoauyy : entity work.o
    port map (xqmuanw => ebxyxm, khw => raxvemoqi, gvrpnr => rpn);
  
  -- Multi-driven assignments
  euqjdh <= (others => '0');
  mhp <= '1';
end yadlmplmli;



-- Seed after: 523645860430445174,6176025545472508329
