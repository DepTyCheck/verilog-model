-- Seed: 13219029354872997387,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity zm is
  port (kubmo : in std_logic_vector(4 to 3); ytn : in time);
end zm;

architecture oozhzp of zm is
  
begin
  
end oozhzp;

entity ae is
  port (yb : inout integer; eoyoqezcqk : inout string(5 to 5));
end ae;

library ieee;
use ieee.std_logic_1164.all;

architecture cymsnclvvy of ae is
  signal uyalrjsgk : time;
  signal uqg : time;
  signal axqj : std_logic_vector(4 to 3);
begin
  rxe : entity work.zm
    port map (kubmo => axqj, ytn => uqg);
  bmuszhnjlv : entity work.zm
    port map (kubmo => axqj, ytn => uqg);
  yysd : entity work.zm
    port map (kubmo => axqj, ytn => uyalrjsgk);
  
  -- Single-driven assignments
  eoyoqezcqk <= (others => 'g');
  uyalrjsgk <= 2#0_0.1101# ns;
  uqg <= 8#1332.45# fs;
  yb <= 4_3;
  
  -- Multi-driven assignments
  axqj <= "";
  axqj <= (others => '0');
  axqj <= (others => '0');
end cymsnclvvy;



-- Seed after: 3781092336159814695,17924494779688682807
