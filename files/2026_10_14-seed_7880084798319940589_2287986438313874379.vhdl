-- Seed: 7880084798319940589,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity oomxvmu is
  port (trgszuwhla : linkage time; ontj : out std_logic; ouov : in time);
end oomxvmu;

architecture mjajlqkrve of oomxvmu is
  
begin
  -- Multi-driven assignments
  ontj <= '1';
end mjajlqkrve;

library ieee;
use ieee.std_logic_1164.all;

entity hq is
  port (eaw : out real; oiq : buffer std_logic_vector(0 to 4); gch : out real; nuxzdrnma : buffer time_vector(2 downto 3));
end hq;

library ieee;
use ieee.std_logic_1164.all;

architecture qfw of hq is
  signal tpnjg : std_logic;
  signal q : time;
  signal vhxzeepyh : time;
  signal fdrjsrp : std_logic;
  signal oumdg : time;
  signal aurou : std_logic;
  signal qerjltwmk : time;
begin
  hb : entity work.oomxvmu
    port map (trgszuwhla => qerjltwmk, ontj => aurou, ouov => qerjltwmk);
  g : entity work.oomxvmu
    port map (trgszuwhla => oumdg, ontj => fdrjsrp, ouov => qerjltwmk);
  zfagsbzoer : entity work.oomxvmu
    port map (trgszuwhla => vhxzeepyh, ontj => aurou, ouov => qerjltwmk);
  ezihh : entity work.oomxvmu
    port map (trgszuwhla => q, ontj => tpnjg, ouov => qerjltwmk);
  
  -- Multi-driven assignments
  oiq <= "L00XX";
  oiq <= oiq;
end qfw;

entity ga is
  port (bilpc : linkage string(4 to 1); f : out real);
end ga;

library ieee;
use ieee.std_logic_1164.all;

architecture hxp of ga is
  signal pdrq : time_vector(2 downto 3);
  signal nzbgoihxhf : real;
  signal nvvfan : time;
  signal bi : time_vector(2 downto 3);
  signal ebbiqomso : real;
  signal lzgr : std_logic_vector(0 to 4);
  signal licde : real;
  signal txu : time;
  signal j : std_logic;
  signal x : time;
begin
  nf : entity work.oomxvmu
    port map (trgszuwhla => x, ontj => j, ouov => txu);
  fifiixyqk : entity work.hq
    port map (eaw => licde, oiq => lzgr, gch => ebbiqomso, nuxzdrnma => bi);
  pnao : entity work.oomxvmu
    port map (trgszuwhla => nvvfan, ontj => j, ouov => txu);
  rthjyfmvu : entity work.hq
    port map (eaw => f, oiq => lzgr, gch => nzbgoihxhf, nuxzdrnma => pdrq);
  
  -- Single-driven assignments
  txu <= x;
  
  -- Multi-driven assignments
  lzgr <= "ZXX-Z";
  j <= j;
end hxp;



-- Seed after: 11437755967775150081,2287986438313874379
