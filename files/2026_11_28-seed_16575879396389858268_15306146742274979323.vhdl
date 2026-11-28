-- Seed: 16575879396389858268,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity ocd is
  port (ieikvhh : inout std_logic; f : linkage boolean);
end ocd;

architecture sl of ocd is
  
begin
  -- Multi-driven assignments
  ieikvhh <= 'W';
  ieikvhh <= 'Z';
  ieikvhh <= ieikvhh;
  ieikvhh <= ieikvhh;
end sl;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (o : out boolean; mntcsufd : in time; oysajn : inout std_logic_vector(2 downto 0); urrpks : in integer);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture r of j is
  signal k : std_logic;
  signal qiwydgs : boolean;
  signal eihnmdb : boolean;
  signal jjbhsry : std_logic;
  signal ngcaa : boolean;
  signal mfdtrexkd : std_logic;
begin
  z : entity work.ocd
    port map (ieikvhh => mfdtrexkd, f => ngcaa);
  jkn : entity work.ocd
    port map (ieikvhh => jjbhsry, f => eihnmdb);
  czinywgt : entity work.ocd
    port map (ieikvhh => mfdtrexkd, f => qiwydgs);
  av : entity work.ocd
    port map (ieikvhh => k, f => o);
  
  -- Multi-driven assignments
  oysajn <= oysajn;
  oysajn <= "LZW";
  jjbhsry <= '-';
end r;



-- Seed after: 477427282893081398,15306146742274979323
