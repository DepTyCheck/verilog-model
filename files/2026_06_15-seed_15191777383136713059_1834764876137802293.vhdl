-- Seed: 15191777383136713059,1834764876137802293

entity mcdav is
  port (scifpsc : buffer integer);
end mcdav;

architecture kyp of mcdav is
  
begin
  -- Single-driven assignments
  scifpsc <= 0_3_1_1;
end kyp;

library ieee;
use ieee.std_logic_1164.all;

entity qjzqeqr is
  port (rckm : linkage time; dbnga : in real; r : buffer std_logic; oiopxbi : in integer);
end qjzqeqr;

architecture ykvc of qjzqeqr is
  signal zxawzd : integer;
  signal nlhsgya : integer;
  signal v : integer;
begin
  ekcxs : entity work.mcdav
    port map (scifpsc => v);
  wlift : entity work.mcdav
    port map (scifpsc => nlhsgya);
  hvg : entity work.mcdav
    port map (scifpsc => zxawzd);
  
  -- Multi-driven assignments
  r <= 'Z';
  r <= 'X';
end ykvc;



-- Seed after: 7975838758989378205,1834764876137802293
