-- Seed: 5279880072592170245,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity fssbxzllqv is
  port (s : buffer time; rtxlew : in std_logic_vector(3 to 3));
end fssbxzllqv;

architecture nwsol of fssbxzllqv is
  
begin
  -- Single-driven assignments
  s <= 4 min;
end nwsol;

library ieee;
use ieee.std_logic_1164.all;

entity tg is
  port (yxjbblh : in integer; uqh : in std_logic);
end tg;

library ieee;
use ieee.std_logic_1164.all;

architecture p of tg is
  signal t : time;
  signal z : std_logic_vector(3 to 3);
  signal kosgj : time;
  signal vahey : std_logic_vector(3 to 3);
  signal ylurno : time;
begin
  emmukbdb : entity work.fssbxzllqv
    port map (s => ylurno, rtxlew => vahey);
  slaxdw : entity work.fssbxzllqv
    port map (s => kosgj, rtxlew => z);
  yknrfy : entity work.fssbxzllqv
    port map (s => t, rtxlew => vahey);
  
  -- Multi-driven assignments
  vahey <= (others => '0');
  vahey <= (others => 'Z');
  vahey <= vahey;
  vahey <= (others => '1');
end p;



-- Seed after: 10825029705513908404,6299883410057943775
