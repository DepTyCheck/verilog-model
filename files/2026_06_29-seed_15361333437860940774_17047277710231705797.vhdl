-- Seed: 15361333437860940774,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity gqmntpnegu is
  port (pvlrl : buffer integer; uzcr : out time; atmgbvkmky : out std_logic; osvx : in integer);
end gqmntpnegu;

architecture soqzzv of gqmntpnegu is
  
begin
  -- Multi-driven assignments
  atmgbvkmky <= 'X';
  atmgbvkmky <= 'W';
  atmgbvkmky <= '1';
  atmgbvkmky <= 'L';
end soqzzv;

entity ykhhtkthh is
  port (ofizsownp : buffer time_vector(1 to 2));
end ykhhtkthh;

architecture ywnblh of ykhhtkthh is
  
begin
  -- Single-driven assignments
  ofizsownp <= (2 min, 0 sec);
end ywnblh;

library ieee;
use ieee.std_logic_1164.all;

entity rulfsnisj is
  port (yglnfp : out time; ffjwtcpaar : in std_logic_vector(1 to 3); nsr : linkage integer);
end rulfsnisj;

library ieee;
use ieee.std_logic_1164.all;

architecture zxh of rulfsnisj is
  signal untfoyfvbg : time_vector(1 to 2);
  signal gtby : time_vector(1 to 2);
  signal qyoj : integer;
  signal sgqanoork : std_logic;
  signal dbnziru : integer;
begin
  q : entity work.gqmntpnegu
    port map (pvlrl => dbnziru, uzcr => yglnfp, atmgbvkmky => sgqanoork, osvx => qyoj);
  kur : entity work.ykhhtkthh
    port map (ofizsownp => gtby);
  vsx : entity work.ykhhtkthh
    port map (ofizsownp => untfoyfvbg);
  
  -- Single-driven assignments
  qyoj <= 0_3;
  
  -- Multi-driven assignments
  sgqanoork <= '1';
end zxh;



-- Seed after: 4787693605078476775,17047277710231705797
