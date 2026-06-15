-- Seed: 8796051494349223938,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity ktk is
  port (hcpzfhew : buffer std_logic);
end ktk;

architecture o of ktk is
  
begin
  -- Multi-driven assignments
  hcpzfhew <= 'U';
  hcpzfhew <= 'U';
  hcpzfhew <= 'H';
end o;

library ieee;
use ieee.std_logic_1164.all;

entity nc is
  port (w : inout std_logic; puefqikw : inout std_logic; ooo : out time_vector(2 downto 4));
end nc;

library ieee;
use ieee.std_logic_1164.all;

architecture wvlvs of nc is
  signal hdfu : std_logic;
  signal oflwouqaqv : std_logic;
begin
  sb : entity work.ktk
    port map (hcpzfhew => puefqikw);
  kzmjwtmlv : entity work.ktk
    port map (hcpzfhew => oflwouqaqv);
  efrxmjcuu : entity work.ktk
    port map (hcpzfhew => w);
  vwzprrrldv : entity work.ktk
    port map (hcpzfhew => hdfu);
  
  -- Single-driven assignments
  ooo <= (others => 0 ns);
  
  -- Multi-driven assignments
  w <= 'U';
  puefqikw <= '-';
end wvlvs;

entity ckhnqvv is
  port (kztspot : linkage real; ezwwld : buffer boolean_vector(0 to 3));
end ckhnqvv;

architecture bqix of ckhnqvv is
  
begin
  -- Single-driven assignments
  ezwwld <= (FALSE, FALSE, FALSE, TRUE);
end bqix;

library ieee;
use ieee.std_logic_1164.all;

entity ijoaeyvub is
  port (wzfluaacb : out std_logic; bjyjgw : inout real; vy : out time; ghssbayzts : linkage std_logic);
end ijoaeyvub;

architecture crhnkb of ijoaeyvub is
  signal nqnscrwb : time_vector(2 downto 4);
begin
  ibec : entity work.ktk
    port map (hcpzfhew => wzfluaacb);
  kiyycjv : entity work.nc
    port map (w => wzfluaacb, puefqikw => wzfluaacb, ooo => nqnscrwb);
  
  -- Single-driven assignments
  vy <= 0_0_4_1 fs;
end crhnkb;



-- Seed after: 17243183688584956403,1834764876137802293
