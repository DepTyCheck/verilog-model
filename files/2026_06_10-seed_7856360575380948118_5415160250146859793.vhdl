-- Seed: 7856360575380948118,5415160250146859793

library ieee;
use ieee.std_logic_1164.all;

entity tfyxwbhe is
  port (ua : inout std_logic; dcmte : in std_logic_vector(0 to 0); usq : out std_logic_vector(1 downto 4));
end tfyxwbhe;



architecture f of tfyxwbhe is
  
begin
  
end f;

library ieee;
use ieee.std_logic_1164.all;

entity tevivc is
  port (zkol : linkage time; i : linkage time; suhodnog : in real; kjxidrtxej : out std_logic);
end tevivc;

library ieee;
use ieee.std_logic_1164.all;

architecture bl of tevivc is
  signal gwmorwi : std_logic_vector(1 downto 4);
  signal pmbsqcsi : std_logic_vector(0 to 0);
  signal fc : std_logic;
begin
  xdbuohlcyp : entity work.tfyxwbhe
    port map (ua => fc, dcmte => pmbsqcsi, usq => gwmorwi);
end bl;



entity xfb is
  port (xcpcggwtw : in severity_level; rwjz : in boolean; klrzlvfm : linkage bit_vector(0 downto 0); yptrqs : linkage integer);
end xfb;

library ieee;
use ieee.std_logic_1164.all;

architecture qucqilphv of xfb is
  signal lgwybnqx : std_logic_vector(1 downto 4);
  signal glb : std_logic;
  signal fode : std_logic_vector(1 downto 4);
  signal hocp : std_logic_vector(0 to 0);
  signal fhehuylf : std_logic;
  signal lmhuf : std_logic;
  signal yiboeuzxdy : real;
  signal fmhbb : time;
  signal hokwcl : time;
begin
  qqudjosyro : entity work.tevivc
    port map (zkol => hokwcl, i => fmhbb, suhodnog => yiboeuzxdy, kjxidrtxej => lmhuf);
  nwosbrvino : entity work.tfyxwbhe
    port map (ua => fhehuylf, dcmte => hocp, usq => fode);
  pmizl : entity work.tfyxwbhe
    port map (ua => glb, dcmte => hocp, usq => fode);
  jxak : entity work.tfyxwbhe
    port map (ua => lmhuf, dcmte => hocp, usq => lgwybnqx);
end qucqilphv;



-- Seed after: 3470836977111372846,5415160250146859793
