-- Seed: 14279232346997254975,1834764876137802293

entity qvmmnplb is
  port (cnmrqrnmxg : in time; yhv : in time; qkjcgrhkis : linkage bit_vector(2 to 2); pm : out boolean);
end qvmmnplb;

architecture xgszfzs of qvmmnplb is
  
begin
  -- Single-driven assignments
  pm <= FALSE;
end xgszfzs;

library ieee;
use ieee.std_logic_1164.all;

entity majhq is
  port (nihrd : linkage std_logic_vector(1 downto 1); rvzwnpag : buffer severity_level);
end majhq;

architecture lcobm of majhq is
  signal lg : boolean;
  signal itkprz : bit_vector(2 to 2);
  signal hhv : boolean;
  signal zwjslljd : bit_vector(2 to 2);
  signal dsvbixp : time;
  signal skhjcccf : time;
begin
  tb : entity work.qvmmnplb
    port map (cnmrqrnmxg => skhjcccf, yhv => dsvbixp, qkjcgrhkis => zwjslljd, pm => hhv);
  shs : entity work.qvmmnplb
    port map (cnmrqrnmxg => skhjcccf, yhv => skhjcccf, qkjcgrhkis => itkprz, pm => lg);
  
  -- Single-driven assignments
  rvzwnpag <= FAILURE;
  dsvbixp <= 320 us;
  skhjcccf <= 1331.0_2_0_0_3 fs;
end lcobm;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (roiesxca : inout integer; qgolgoyn : buffer std_logic_vector(4 downto 4); hlve : in std_logic; p : inout std_logic_vector(0 to 2));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture z of n is
  signal kgbmuxzgyf : severity_level;
  signal tpj : std_logic_vector(1 downto 1);
  signal awpm : severity_level;
  signal d : std_logic_vector(1 downto 1);
  signal sebysb : boolean;
  signal xic : bit_vector(2 to 2);
  signal sefg : time;
  signal tlqyybqnhv : time;
  signal lpwvfjev : boolean;
  signal keoquv : bit_vector(2 to 2);
  signal xus : time;
  signal gtuirrhm : time;
begin
  kclmytzb : entity work.qvmmnplb
    port map (cnmrqrnmxg => gtuirrhm, yhv => xus, qkjcgrhkis => keoquv, pm => lpwvfjev);
  pdhbxznzz : entity work.qvmmnplb
    port map (cnmrqrnmxg => tlqyybqnhv, yhv => sefg, qkjcgrhkis => xic, pm => sebysb);
  go : entity work.majhq
    port map (nihrd => d, rvzwnpag => awpm);
  ptksq : entity work.majhq
    port map (nihrd => tpj, rvzwnpag => kgbmuxzgyf);
  
  -- Multi-driven assignments
  d <= "1";
  p <= ('U', 'H', 'L');
  p <= "-H1";
end z;

entity zq is
  port (iaercvxza : out boolean; cmj : inout time);
end zq;

architecture dond of zq is
  signal arztb : boolean;
  signal jn : bit_vector(2 to 2);
  signal egfd : time;
  signal zjtn : bit_vector(2 to 2);
begin
  hbetoscjb : entity work.qvmmnplb
    port map (cnmrqrnmxg => cmj, yhv => cmj, qkjcgrhkis => zjtn, pm => iaercvxza);
  wzfrtyy : entity work.qvmmnplb
    port map (cnmrqrnmxg => cmj, yhv => egfd, qkjcgrhkis => jn, pm => arztb);
  
  -- Single-driven assignments
  cmj <= 8#5.05226# ms;
  egfd <= 1 hr;
end dond;



-- Seed after: 11687748402299190165,1834764876137802293
