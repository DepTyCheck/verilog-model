-- Seed: 1077571269853706908,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity htnktwwp is
  port (ngjdipus : out std_logic_vector(4 to 4); n : in integer);
end htnktwwp;

architecture qcxdphkz of htnktwwp is
  
begin
  
end qcxdphkz;

entity wzqdflrg is
  port (ctdzlillvj : buffer integer; hov : buffer character);
end wzqdflrg;

library ieee;
use ieee.std_logic_1164.all;

architecture zzlemrgb of wzqdflrg is
  signal pnbd : integer;
  signal iddtglpdt : std_logic_vector(4 to 4);
  signal nrgwjnqert : integer;
  signal purmn : std_logic_vector(4 to 4);
  signal dywey : integer;
  signal rhg : std_logic_vector(4 to 4);
begin
  ztomokbfon : entity work.htnktwwp
    port map (ngjdipus => rhg, n => dywey);
  zn : entity work.htnktwwp
    port map (ngjdipus => purmn, n => nrgwjnqert);
  y : entity work.htnktwwp
    port map (ngjdipus => iddtglpdt, n => pnbd);
  
  -- Single-driven assignments
  hov <= 'l';
  nrgwjnqert <= 3;
  pnbd <= 2#100#;
  dywey <= 2#11#;
  ctdzlillvj <= nrgwjnqert;
  
  -- Multi-driven assignments
  iddtglpdt <= iddtglpdt;
end zzlemrgb;



-- Seed after: 15060456333232450122,3426943171337919827
