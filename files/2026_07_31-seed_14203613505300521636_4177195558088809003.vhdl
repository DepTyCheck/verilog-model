-- Seed: 14203613505300521636,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (zclklpokp : in time; vmrl : out real_vector(0 to 2); zcxpm : buffer time; czgaazhghe : linkage std_logic_vector(2 downto 3));
end m;

architecture lyjcrhhcx of m is
  
begin
  -- Single-driven assignments
  zcxpm <= zcxpm;
  vmrl <= (1.2, 21200.2, 8#7_3.4#);
end lyjcrhhcx;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (hc : out std_logic_vector(3 downto 0));
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture covwdrhnic of t is
  signal fyr : std_logic_vector(2 downto 3);
  signal ra : time;
  signal zhylbdik : real_vector(0 to 2);
  signal urv : time;
  signal ombycnprg : std_logic_vector(2 downto 3);
  signal vfjyavxyi : time;
  signal pfdibl : real_vector(0 to 2);
  signal lsahcih : time;
  signal kyufp : real_vector(0 to 2);
  signal s : time;
  signal gzipics : std_logic_vector(2 downto 3);
  signal apvpngzso : real_vector(0 to 2);
  signal jwu : time;
begin
  y : entity work.m
    port map (zclklpokp => jwu, vmrl => apvpngzso, zcxpm => jwu, czgaazhghe => gzipics);
  quvvajtsr : entity work.m
    port map (zclklpokp => s, vmrl => kyufp, zcxpm => s, czgaazhghe => gzipics);
  af : entity work.m
    port map (zclklpokp => lsahcih, vmrl => pfdibl, zcxpm => vfjyavxyi, czgaazhghe => ombycnprg);
  gbzvcxjpy : entity work.m
    port map (zclklpokp => urv, vmrl => zhylbdik, zcxpm => ra, czgaazhghe => fyr);
  
  -- Multi-driven assignments
  hc <= ('H', '1', 'Z', '1');
  fyr <= fyr;
  gzipics <= (others => '0');
  fyr <= gzipics;
end covwdrhnic;

library ieee;
use ieee.std_logic_1164.all;

entity dkbx is
  port (nsljxtk : linkage std_logic);
end dkbx;

library ieee;
use ieee.std_logic_1164.all;

architecture k of dkbx is
  signal bqagfxha : std_logic_vector(2 downto 3);
  signal u : real_vector(0 to 2);
  signal pftbx : time;
begin
  pagfeswv : entity work.m
    port map (zclklpokp => pftbx, vmrl => u, zcxpm => pftbx, czgaazhghe => bqagfxha);
  
  -- Multi-driven assignments
  bqagfxha <= bqagfxha;
  bqagfxha <= (others => '0');
  bqagfxha <= bqagfxha;
end k;



-- Seed after: 10323894889863919956,4177195558088809003
