-- Seed: 5240567020665533259,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity igzmaofa is
  port (mafx : out real; lvykqn : in integer; xtqo : linkage time; gdlsx : in std_logic_vector(3 to 3));
end igzmaofa;

architecture levqvm of igzmaofa is
  
begin
  -- Single-driven assignments
  mafx <= 2#1110.00#;
end levqvm;

library ieee;
use ieee.std_logic_1164.all;

entity zmpeodreos is
  port (i : buffer std_logic; qkawj : inout std_logic; oximz : linkage boolean; g : buffer time);
end zmpeodreos;

library ieee;
use ieee.std_logic_1164.all;

architecture sv of zmpeodreos is
  signal vstm : std_logic_vector(3 to 3);
  signal u : time;
  signal ggwupnydye : integer;
  signal nrhbzbovxy : real;
begin
  gmlnz : entity work.igzmaofa
    port map (mafx => nrhbzbovxy, lvykqn => ggwupnydye, xtqo => u, gdlsx => vstm);
  
  -- Multi-driven assignments
  vstm <= (others => 'W');
  i <= '1';
end sv;

entity gaj is
  port (lqekvra : out integer; k : inout time);
end gaj;

library ieee;
use ieee.std_logic_1164.all;

architecture vfdhx of gaj is
  signal dvkp : std_logic_vector(3 to 3);
  signal q : time;
  signal wcf : real;
  signal nxqzhp : std_logic_vector(3 to 3);
  signal sxulql : real;
begin
  xainqjv : entity work.igzmaofa
    port map (mafx => sxulql, lvykqn => lqekvra, xtqo => k, gdlsx => nxqzhp);
  skjapt : entity work.igzmaofa
    port map (mafx => wcf, lvykqn => lqekvra, xtqo => q, gdlsx => dvkp);
  
  -- Single-driven assignments
  lqekvra <= 2_1_2_0_0;
end vfdhx;



-- Seed after: 4096471212822837087,1834764876137802293
