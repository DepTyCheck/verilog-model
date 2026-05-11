-- Seed: 12301163313873769408,9372850727389630639

library ieee;
use ieee.std_logic_1164.all;

entity av is
  port (rtwb : linkage std_logic);
end av;



architecture oku of av is
  
begin
  
end oku;



entity pfs is
  port (enhdv : inout integer; ta : out time);
end pfs;

library ieee;
use ieee.std_logic_1164.all;

architecture jno of pfs is
  signal ybtimhk : std_logic;
  signal mm : std_logic;
begin
  ieksrctgt : entity work.av
    port map (rtwb => mm);
  vzgus : entity work.av
    port map (rtwb => mm);
  rug : entity work.av
    port map (rtwb => ybtimhk);
end jno;



-- Seed after: 6284890968672486327,9372850727389630639
