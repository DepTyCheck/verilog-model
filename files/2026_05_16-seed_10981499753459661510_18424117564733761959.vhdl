-- Seed: 10981499753459661510,18424117564733761959

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (uccvui : buffer std_logic; ck : buffer time);
end u;



architecture qz of u is
  
begin
  
end qz;



entity bvbfax is
  port (zj : in time; vkiebrv : buffer real; mgot : in bit);
end bvbfax;

library ieee;
use ieee.std_logic_1164.all;

architecture onwjv of bvbfax is
  signal djeewqxc : time;
  signal co : std_logic;
begin
  epmr : entity work.u
    port map (uccvui => co, ck => djeewqxc);
end onwjv;



entity xun is
  port (bxs : linkage time; xukkuwdo : buffer real);
end xun;

library ieee;
use ieee.std_logic_1164.all;

architecture cx of xun is
  signal bduvlcqsl : real;
  signal rzirpx : time;
  signal qfeg : std_logic;
  signal vydkttz : bit;
  signal adnrfdzdk : time;
begin
  j : entity work.bvbfax
    port map (zj => adnrfdzdk, vkiebrv => xukkuwdo, mgot => vydkttz);
  mraixgxahy : entity work.u
    port map (uccvui => qfeg, ck => rzirpx);
  ebl : entity work.bvbfax
    port map (zj => adnrfdzdk, vkiebrv => bduvlcqsl, mgot => vydkttz);
end cx;



-- Seed after: 5378195310692942722,18424117564733761959
