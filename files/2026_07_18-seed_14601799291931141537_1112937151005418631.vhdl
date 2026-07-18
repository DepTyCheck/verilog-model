-- Seed: 14601799291931141537,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity uvi is
  port (akpsue : inout std_logic_vector(4 downto 3));
end uvi;

architecture kfr of uvi is
  
begin
  -- Multi-driven assignments
  akpsue <= ('Z', '0');
end kfr;

entity ug is
  port (orzodtf : buffer severity_level);
end ug;

library ieee;
use ieee.std_logic_1164.all;

architecture zkl of ug is
  signal lknshgmh : std_logic_vector(4 downto 3);
  signal xwosl : std_logic_vector(4 downto 3);
  signal hpnppaeqmi : std_logic_vector(4 downto 3);
begin
  vjdjahtg : entity work.uvi
    port map (akpsue => hpnppaeqmi);
  nwtlhylzdo : entity work.uvi
    port map (akpsue => hpnppaeqmi);
  qquaplvbt : entity work.uvi
    port map (akpsue => xwosl);
  smueig : entity work.uvi
    port map (akpsue => lknshgmh);
  
  -- Multi-driven assignments
  xwosl <= "LX";
  hpnppaeqmi <= "1-";
end zkl;



-- Seed after: 7148137236877438674,1112937151005418631
