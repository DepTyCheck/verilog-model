-- Seed: 17540253902924588720,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity onoihtcvno is
  port (fz : inout std_logic_vector(1 downto 0); obhzy : buffer integer; etzgp : out severity_level; cluno : buffer bit);
end onoihtcvno;

architecture ulmvhyxx of onoihtcvno is
  
begin
  -- Multi-driven assignments
  fz <= "LL";
  fz <= fz;
  fz <= fz;
  fz <= fz;
end ulmvhyxx;

library ieee;
use ieee.std_logic_1164.all;

entity jwtkx is
  port (lpaujxhh : inout string(3 to 3); kburgz : inout bit_vector(4 downto 4); oqmgyda : buffer real; sae : in std_logic_vector(3 downto 2));
end jwtkx;

architecture mpbob of jwtkx is
  
begin
  -- Single-driven assignments
  oqmgyda <= 16#1_9_8.F_F_6_7_0#;
  kburgz <= kburgz;
  lpaujxhh <= lpaujxhh;
end mpbob;

library ieee;
use ieee.std_logic_1164.all;

entity wvchx is
  port (opwzuq : buffer std_logic_vector(3 to 3); tgow : buffer time_vector(4 to 2));
end wvchx;

library ieee;
use ieee.std_logic_1164.all;

architecture sspvycpvfy of wvchx is
  signal cfrhadvztf : bit;
  signal pid : severity_level;
  signal lso : integer;
  signal mwrcdyre : std_logic_vector(1 downto 0);
  signal lkgwud : real;
  signal qnnge : bit_vector(4 downto 4);
  signal ilklxyim : string(3 to 3);
  signal gqhm : bit;
  signal czorkqjdw : severity_level;
  signal aoppsytam : integer;
  signal somqowi : std_logic_vector(3 downto 2);
  signal sagtzdq : real;
  signal iyfzuz : bit_vector(4 downto 4);
  signal j : string(3 to 3);
begin
  gw : entity work.jwtkx
    port map (lpaujxhh => j, kburgz => iyfzuz, oqmgyda => sagtzdq, sae => somqowi);
  lve : entity work.onoihtcvno
    port map (fz => somqowi, obhzy => aoppsytam, etzgp => czorkqjdw, cluno => gqhm);
  ixmobpxyqr : entity work.jwtkx
    port map (lpaujxhh => ilklxyim, kburgz => qnnge, oqmgyda => lkgwud, sae => somqowi);
  gxwmcdgdj : entity work.onoihtcvno
    port map (fz => mwrcdyre, obhzy => lso, etzgp => pid, cluno => cfrhadvztf);
  
  -- Single-driven assignments
  tgow <= (others => 0 ns);
  
  -- Multi-driven assignments
  somqowi <= ('X', '0');
end sspvycpvfy;



-- Seed after: 2439384889872865157,6299883410057943775
