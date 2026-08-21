-- Seed: 13691027118004993462,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (zhmmot : linkage integer; oif : linkage std_logic; exnibinmi : inout integer; gumdemrzx : linkage std_logic);
end i;

architecture k of i is
  
begin
  -- Single-driven assignments
  exnibinmi <= 2;
end k;

library ieee;
use ieee.std_logic_1164.all;

entity mfabxtxx is
  port (nccjwhr : inout integer; mmrn : buffer integer; buocitxj : out integer; dxwp : in std_logic);
end mfabxtxx;

library ieee;
use ieee.std_logic_1164.all;

architecture nscmt of mfabxtxx is
  signal ybm : integer;
  signal fveeukge : std_logic;
  signal xv : integer;
  signal o : std_logic;
  signal qkqqd : integer;
  signal rj : std_logic;
  signal cbmytlxa : integer;
  signal badg : std_logic;
  signal ooystzvyt : integer;
begin
  qjduks : entity work.i
    port map (zhmmot => ooystzvyt, oif => badg, exnibinmi => cbmytlxa, gumdemrzx => rj);
  qy : entity work.i
    port map (zhmmot => qkqqd, oif => o, exnibinmi => xv, gumdemrzx => fveeukge);
  zibhtzsmb : entity work.i
    port map (zhmmot => buocitxj, oif => dxwp, exnibinmi => mmrn, gumdemrzx => dxwp);
  xsn : entity work.i
    port map (zhmmot => ybm, oif => rj, exnibinmi => nccjwhr, gumdemrzx => dxwp);
end nscmt;

library ieee;
use ieee.std_logic_1164.all;

entity lyfaptupx is
  port (koczwmr : in character; k : buffer std_logic_vector(4 to 1));
end lyfaptupx;

library ieee;
use ieee.std_logic_1164.all;

architecture iyonv of lyfaptupx is
  signal jkybeslp : integer;
  signal dtwlbrd : std_logic;
  signal najssm : integer;
  signal aeyls : integer;
  signal jmeefy : integer;
  signal bxfthrone : integer;
  signal ggcfvwv : std_logic;
  signal tg : integer;
  signal qqyxktoyp : std_logic;
  signal emgej : integer;
begin
  tdinyviq : entity work.i
    port map (zhmmot => emgej, oif => qqyxktoyp, exnibinmi => tg, gumdemrzx => ggcfvwv);
  mexkjvbhlp : entity work.mfabxtxx
    port map (nccjwhr => bxfthrone, mmrn => jmeefy, buocitxj => aeyls, dxwp => qqyxktoyp);
  dsknyahed : entity work.i
    port map (zhmmot => najssm, oif => dtwlbrd, exnibinmi => jkybeslp, gumdemrzx => ggcfvwv);
  
  -- Multi-driven assignments
  qqyxktoyp <= 'X';
  k <= k;
  k <= "";
  dtwlbrd <= qqyxktoyp;
end iyonv;



-- Seed after: 16596419625332409956,16188444798499499427
