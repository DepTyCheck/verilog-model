-- Seed: 15603692187520494891,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity jvh is
  port (yazserzz : out std_logic_vector(1 to 1); gt : linkage time; vdihysh : linkage real);
end jvh;

architecture mrwsuz of jvh is
  
begin
  -- Multi-driven assignments
  yazserzz <= (others => '0');
  yazserzz <= (others => 'X');
end mrwsuz;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (s : inout std_logic);
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture stncqb of t is
  signal zjgjbzlvbu : real;
  signal rrwytdyfbu : time;
  signal dnuxe : real;
  signal gloinmrum : time;
  signal hqmmgx : std_logic_vector(1 to 1);
begin
  ogpzun : entity work.jvh
    port map (yazserzz => hqmmgx, gt => gloinmrum, vdihysh => dnuxe);
  uo : entity work.jvh
    port map (yazserzz => hqmmgx, gt => rrwytdyfbu, vdihysh => zjgjbzlvbu);
  
  -- Multi-driven assignments
  s <= 'H';
  s <= '0';
end stncqb;



-- Seed after: 7764606663808603291,17924494779688682807
