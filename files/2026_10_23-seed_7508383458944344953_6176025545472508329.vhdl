-- Seed: 7508383458944344953,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity cmxp is
  port (wmik : out std_logic; j : out std_logic; syefxw : buffer integer; cihmrh : in time);
end cmxp;

architecture kppne of cmxp is
  
begin
  -- Multi-driven assignments
  wmik <= 'Z';
  j <= j;
  wmik <= wmik;
end kppne;

library ieee;
use ieee.std_logic_1164.all;

entity bxu is
  port (axsxlc : out std_logic_vector(0 to 0); njfulcs : out std_logic_vector(2 to 3); wb : linkage bit);
end bxu;

library ieee;
use ieee.std_logic_1164.all;

architecture hyky of bxu is
  signal bu : time;
  signal ygtefkvar : integer;
  signal eiyn : time;
  signal mhomggdzdh : integer;
  signal iott : std_logic;
  signal mddlwg : std_logic;
  signal qveigg : time;
  signal fabbuzk : integer;
  signal yylyks : std_logic;
  signal q : std_logic;
begin
  w : entity work.cmxp
    port map (wmik => q, j => yylyks, syefxw => fabbuzk, cihmrh => qveigg);
  clk : entity work.cmxp
    port map (wmik => mddlwg, j => iott, syefxw => mhomggdzdh, cihmrh => eiyn);
  mmew : entity work.cmxp
    port map (wmik => iott, j => q, syefxw => ygtefkvar, cihmrh => bu);
  
  -- Single-driven assignments
  qveigg <= qveigg;
  bu <= 4_0_1.33 ps;
  eiyn <= qveigg;
  
  -- Multi-driven assignments
  njfulcs <= njfulcs;
end hyky;



-- Seed after: 1216996475978657820,6176025545472508329
