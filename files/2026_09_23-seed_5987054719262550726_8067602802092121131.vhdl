-- Seed: 5987054719262550726,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity ihfybank is
  port (jqduzeacxn : buffer severity_level; j : out std_logic; nglzczzszp : out time; ulmnullt : in std_logic);
end ihfybank;

architecture ysxxu of ihfybank is
  
begin
  -- Single-driven assignments
  nglzczzszp <= 16#9_E_9_E_1.07AF# ps;
  jqduzeacxn <= jqduzeacxn;
  
  -- Multi-driven assignments
  j <= 'X';
  j <= ulmnullt;
  j <= 'W';
end ysxxu;

entity vf is
  port (mvylkvonf : out time);
end vf;

library ieee;
use ieee.std_logic_1164.all;

architecture jvg of vf is
  signal tcr : std_logic;
  signal zjcjm : time;
  signal cnlqwnsrte : severity_level;
  signal w : std_logic;
  signal zdjupq : std_logic;
  signal bhg : severity_level;
begin
  xf : entity work.ihfybank
    port map (jqduzeacxn => bhg, j => zdjupq, nglzczzszp => mvylkvonf, ulmnullt => w);
  lrgroav : entity work.ihfybank
    port map (jqduzeacxn => cnlqwnsrte, j => zdjupq, nglzczzszp => zjcjm, ulmnullt => tcr);
  
  -- Multi-driven assignments
  w <= zdjupq;
  tcr <= tcr;
  zdjupq <= '-';
end jvg;



-- Seed after: 5974402714531312531,8067602802092121131
