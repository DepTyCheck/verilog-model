-- Seed: 11839581091668492407,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity utbavp is
  port (ok : out std_logic; hq : in time; hbdbyrnjbm : out std_logic);
end utbavp;

architecture dcqlqqeced of utbavp is
  
begin
  -- Multi-driven assignments
  hbdbyrnjbm <= 'U';
  hbdbyrnjbm <= '-';
  ok <= hbdbyrnjbm;
  hbdbyrnjbm <= '1';
end dcqlqqeced;

library ieee;
use ieee.std_logic_1164.all;

entity ceufcdj is
  port (wkbzmdw : linkage std_logic_vector(4 downto 1); zkmrvode : inout integer);
end ceufcdj;

library ieee;
use ieee.std_logic_1164.all;

architecture xsfotbvc of ceufcdj is
  signal dk : std_logic;
  signal b : time;
  signal nxwfoopr : std_logic;
  signal dscqllaxr : std_logic;
  signal vjqcrblh : std_logic;
  signal xpcatogamt : std_logic;
  signal uwmtdazy : time;
  signal vjosnqw : std_logic;
begin
  cgojx : entity work.utbavp
    port map (ok => vjosnqw, hq => uwmtdazy, hbdbyrnjbm => vjosnqw);
  qzprf : entity work.utbavp
    port map (ok => xpcatogamt, hq => uwmtdazy, hbdbyrnjbm => vjqcrblh);
  ymmiy : entity work.utbavp
    port map (ok => dscqllaxr, hq => uwmtdazy, hbdbyrnjbm => vjosnqw);
  yxxeomjcek : entity work.utbavp
    port map (ok => nxwfoopr, hq => b, hbdbyrnjbm => dk);
  
  -- Single-driven assignments
  uwmtdazy <= uwmtdazy;
  zkmrvode <= 2_1_1;
  
  -- Multi-driven assignments
  xpcatogamt <= 'X';
  dscqllaxr <= 'W';
  vjosnqw <= dscqllaxr;
end xsfotbvc;



-- Seed after: 1971165643546384551,3426943171337919827
